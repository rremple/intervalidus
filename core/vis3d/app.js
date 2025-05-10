import * as THREE from 'three';
import { OrbitControls } from 'three/addons/controls/OrbitControls.js';

// 3D visualizer, 100% vibe-coded using Gemini 2.5 Pro Preview 05-06
// Renders the numerically boxed representation of data, allowing it to be rotated, sliced, and understood.
// Launched by core/src/test/scala/intervalidus/Visualize3D.scala

try {
    const urlParams = new URLSearchParams(window.location.search);
    const customTitle = urlParams.get('title');
    if (customTitle && customTitle.trim() !== '') {
        document.title = decodeURIComponent(customTitle.trim());
        //console.log(`Set page title to: "${document.title}"`);
    } else {
        //console.log("No custom title parameter found or it was empty. Using default title.");
    }
} catch (error) {
    console.error("Error processing title URL parameter:", error);
}

let scene, camera, renderer, controls;
let allVisualsGroup;
let slicePlaneHelper;
let dataBoxes = [];
let sceneMinBounds, sceneMaxBounds, sceneSize; // Added sceneSize
let projectionVisualsGroup;
let axisVisualsGroup; // Group for AxesHelper and tick labels

// --- UI Elements ---
const sliceAxisSelect = document.getElementById('sliceAxis');
const sliceCoordinateSlider = document.getElementById('sliceCoordinate');
const sliceCoordValueSpan = document.getElementById('sliceCoordValue');
const showSlicePlaneCheckbox = document.getElementById('showSlicePlane');
const show2DProjectionCheckbox = document.getElementById('show2DProjection');

// --- Constants ---
const LABEL_FONT_SIZE = 20;
const PROJECTION_LABEL_FONT_SIZE = 18;
const AXIS_TICK_LABEL_FONT_SIZE = 14; // Font size for axis tick labels
const AXIS_TICK_LABEL_COLOR = 'lightgray';
const LABEL_TEXT_COLOR = 'white';
const LABEL_BG_COLOR = 'rgba(0, 0, 0, 0.6)';
const BOX_HELPER_COLOR = 0x00ff00;
const PROJECTION_OUTLINE_COLOR = 0xffa500;
const PROJECTION_TEXT_COLOR = 'black';
const PROJECTION_TEXT_BG_COLOR = 'rgba(255, 165, 0, 0.7)';
const SLICE_PLANE_VISIBILITY_OFFSET = 0.005;
const AXIS_LABEL_OFFSET = 0.5; // How far axis labels are offset from the axis line
const AXIS_TICK_LINE_LENGTH = 0.2; // Length of the small tick mark lines, scaled later
const AXIS_TICK_LINE_COLOR = 0xaaaaaa;
const AXIS_TICK_LINE_BASE_LENGTH = 0.2; // Base length for tick marks

const MIN_AXIS_LENGTH_FACTOR = 1.2; // Axes extend at least this factor beyond max data coord from origin
const TARGET_TICKS_PER_AXIS_SIDE = 4; // Aim for roughly this many ticks on positive/negative side
// --- Dragging State ---
const raycaster = new THREE.Raycaster(); const mouse = new THREE.Vector2();
let isDraggingSlicePlane = false; let dragPlane = new THREE.Plane();
let dragStartPointOnHelper = new THREE.Vector3(); let dragInitialCoordinate = 0;
let dragActiveAxis = null; const planeIntersectPoint = new THREE.Vector3();

// --- Main ---
init();
animate();

// --- Three.js Setup ---
function init() {
    scene = new THREE.Scene();
    scene.background = new THREE.Color(0x222222);

    defineDataBoxes(); // Calculates sceneMinBounds, sceneMaxBounds, sceneSize

    const center = new THREE.Vector3();
    new THREE.Box3(sceneMinBounds, sceneMaxBounds).getCenter(center);
    const maxDim = Math.max(sceneSize.x, sceneSize.y, sceneSize.z, 1);

    camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, Math.max(2000, maxDim * 5)); // Increased far plane
    camera.position.set(center.x + maxDim * 1.1, center.y + maxDim * 1.1, center.z + maxDim * 1.8); // Adjusted camera pos
    camera.lookAt(center);

    renderer = new THREE.WebGLRenderer({ antialias: true });
    renderer.setSize(window.innerWidth, window.innerHeight);
    document.getElementById('container').appendChild(renderer.domElement);

    controls = new OrbitControls(camera, renderer.domElement);
    controls.target.copy(center);
    // --- ADJUST CONTROL SPEEDS HERE ---
    controls.zoomSpeed = 10; // Custom zoom speed
    // controls.panSpeed = 1.5;   // Optional: Pan 1.5x faster
    // controls.rotateSpeed = 1.2; // Optional: Rotate 1.2x faster

    // To enable damping for smoother movement:
    // controls.enableDamping = true;
    // controls.dampingFactor = 0.1; // Adjust as needed
    controls.update();

    const ambientLight = new THREE.AmbientLight(0xffffff, 0.8);
    scene.add(ambientLight);
    const directionalLight = new THREE.DirectionalLight(0xffffff, 0.7);
    directionalLight.position.set(1, 1.5, 1).normalize();
    scene.add(directionalLight);

    allVisualsGroup = new THREE.Group();
    dataBoxes.forEach(boxData => allVisualsGroup.add(createBoxVisual(boxData)));
    scene.add(allVisualsGroup);

    projectionVisualsGroup = new THREE.Group();
    scene.add(projectionVisualsGroup);

    const planeGeom = new THREE.PlaneGeometry(1, 1);
    const planeMat = new THREE.MeshBasicMaterial({
        color: 0x00ffff, side: THREE.DoubleSide, transparent: true,
        opacity: 0.2, depthWrite: false
    });
    slicePlaneHelper = new THREE.Mesh(planeGeom, planeMat);
    slicePlaneHelper.name = "SlicePlaneHelper";
    scene.add(slicePlaneHelper);
    slicePlaneHelper.visible = false;

    createAxisVisuals(); // Create axes helper and tick labels

    // Event Listeners
    sliceAxisSelect.addEventListener('change', () => { updateSliderRange(); updateSlice(); });
    sliceCoordinateSlider.addEventListener('input', updateSlice);
    sliceCoordinateSlider.addEventListener('change', updateSlice);
    showSlicePlaneCheckbox.addEventListener('change', updateSlice);
    show2DProjectionCheckbox.addEventListener('change', updateSlice);
    window.addEventListener('resize', onWindowResize);
    renderer.domElement.addEventListener('mousedown', onDocumentMouseDown, false);
    renderer.domElement.addEventListener('mousemove', onDocumentMouseMove, false);
    renderer.domElement.addEventListener('mouseup', onDocumentMouseUp, false);
    renderer.domElement.addEventListener('mouseout', onDocumentMouseUp, false);

    updateSliderRange();
    updateSlice();
}

// --- Data Definition ---
function defineDataBoxes() {
    let customDataProvided = false;
    try {
        const urlParams = new URLSearchParams(window.location.search);
        const dataParam = urlParams.get('data');
        if (dataParam) {
            const parsedData = JSON.parse(decodeURIComponent(dataParam));
            if (Array.isArray(parsedData) && parsedData.length > 0) {
                const validatedBoxes = [];
                for (const item of parsedData) {
                    if (item && item.min && Array.isArray(item.min) && item.min.length === 3 &&
                        item.max && Array.isArray(item.max) && item.max.length === 3 &&
                        typeof item.text === 'string') {
                        validatedBoxes.push({
                            minBounds: new THREE.Vector3().fromArray(item.min),
                            maxBounds: new THREE.Vector3().fromArray(item.max),
                            text: item.text
                        });
                    } else { console.warn("Invalid item in URL data:", item); }
                }
                if (validatedBoxes.length > 0) {
                    dataBoxes = validatedBoxes; customDataProvided = true; //console.log("Loaded data from URL.");
                } else { console.warn("URL data valid JSON but no valid boxes. Using default."); }
            } else { console.warn("URL data not valid array. Using default."); }
        }
    } catch (error) { console.error("Error parsing URL data:", error, ". Using default data."); }

    if (!customDataProvided) {
        //console.log("Using default data set.");
        dataBoxes = [
            { minBounds: new THREE.Vector3(0,0,0), maxBounds: new THREE.Vector3(5,5,5), text: "Hello gyp" },
            { minBounds: new THREE.Vector3(5,0,0), maxBounds: new THREE.Vector3(10,5,5), text: "World qjp" },
            { minBounds: new THREE.Vector3(2,6,2), maxBounds: new THREE.Vector3(8,10,8), text: "3D Data XYZ" },
            { minBounds: new THREE.Vector3(-5,-5,-2), maxBounds: new THREE.Vector3(0,0,3), text: "Zone Alpha" },
            { minBounds: new THREE.Vector3(6,-4,1), maxBounds: new THREE.Vector3(9,-1,4), text: "Region Beta" },
            { minBounds: new THREE.Vector3(3,3,-3), maxBounds: new THREE.Vector3(7,7,2), text: "Slice Me!" }
        ];
    }

    sceneMinBounds = new THREE.Vector3(Infinity,Infinity,Infinity);
    sceneMaxBounds = new THREE.Vector3(-Infinity,-Infinity,-Infinity);
    if (dataBoxes.length > 0) {
        dataBoxes.forEach(box => { sceneMinBounds.min(box.minBounds); sceneMaxBounds.max(box.maxBounds); });
    } else {
        console.warn("No data boxes. Setting default bounds.");
        sceneMinBounds.set(-1,-1,-1); sceneMaxBounds.set(1,1,1);
        dataBoxes.push({ minBounds: new THREE.Vector3(-0.5,-0.5,-0.5), maxBounds: new THREE.Vector3(0.5,0.5,0.5), text: "No Data" });
        sceneMinBounds.min(dataBoxes[0].minBounds); sceneMaxBounds.max(dataBoxes[0].maxBounds);
    }
    // Calculate overall size of the data bounding box
    sceneSize = new THREE.Vector3().subVectors(sceneMaxBounds, sceneMinBounds);
}

// --- Visualization Creation ---
function createTextLabel(text, position, fontSize = LABEL_FONT_SIZE, textColor = LABEL_TEXT_COLOR, bgColor = null, textScaleFactor = 1.0, bold = true) { // Added bold param, default bgColor to null
    const canvas = document.createElement('canvas'); const context = canvas.getContext('2d');
    const fontWeight = bold ? 'Bold ' : '';
    context.font = `${fontWeight}${fontSize}px Arial`; const textMetrics = context.measureText(text);
    const textWidth = textMetrics.width; const vPad = fontSize * 0.4; const hPad = fontSize * (bgColor ? 0.3 : 0); // No horizontal padding if no BG
    canvas.width = Math.ceil(textWidth + hPad * 2); canvas.height = Math.ceil(fontSize + vPad * 2);
    if (bgColor) { // Only draw background if bgColor is provided
        context.fillStyle = bgColor; context.beginPath(); const r = fontSize / 2.5;
        context.moveTo(r, 0); context.lineTo(canvas.width - r, 0); context.quadraticCurveTo(canvas.width, 0, canvas.width, r);
        context.lineTo(canvas.width, canvas.height - r); context.quadraticCurveTo(canvas.width, canvas.height, canvas.width - r, canvas.height);
        context.lineTo(r, canvas.height); context.quadraticCurveTo(0, canvas.height, 0, canvas.height - r);
        context.lineTo(0, r); context.quadraticCurveTo(0, 0, r, 0); context.closePath(); context.fill();
    }
    context.font = `${fontWeight}${fontSize}px Arial`; context.fillStyle = textColor; context.textAlign = 'center';
    context.textBaseline = 'middle'; context.fillText(text, canvas.width / 2, canvas.height / 2);
    const texture = new THREE.CanvasTexture(canvas); texture.needsUpdate = true;
    const material = new THREE.SpriteMaterial({ map: texture, transparent: true, depthTest: true, depthWrite: true, sizeAttenuation: true });
    const sprite = new THREE.Sprite(material);
    const spriteH = 0.6 * textScaleFactor * (fontSize / LABEL_FONT_SIZE); // Scale sprite height a bit based on font size
    sprite.scale.set((spriteH * canvas.width) / canvas.height, spriteH, 1);
    sprite.position.copy(position); return sprite;
}
function createBoxVisual(boxData) {
    const group = new THREE.Group(); group.userData.boxData = boxData;
    const box3 = new THREE.Box3(boxData.minBounds, boxData.maxBounds);
    const boxHelper = new THREE.Box3Helper(box3, BOX_HELPER_COLOR); group.add(boxHelper);
    const center = new THREE.Vector3(); box3.getCenter(center);
    const label = createTextLabel(boxData.text, center, LABEL_FONT_SIZE, LABEL_TEXT_COLOR, LABEL_BG_COLOR, 1.0);
    group.add(label); return group;
}

// Helper function to calculate "nice" step for axis ticks
function getNiceTickStep(maxValue) {
    if (maxValue === 0) return 1; // Default step if range is zero
    const exponent = Math.floor(Math.log10(maxValue));
    const significand = maxValue / Math.pow(10, exponent);

    let niceSignificand;
    if (significand <= 1.0) niceSignificand = 1.0;
    else if (significand <= 2.0) niceSignificand = 2.0;
    else if (significand <= 5.0) niceSignificand = 5.0;
    else niceSignificand = 10.0;

    // Aim for TARGET_TICKS_PER_AXIS_SIDE ticks
    let step = (niceSignificand * Math.pow(10, exponent)) / TARGET_TICKS_PER_AXIS_SIDE;

    // Further refine step to be a "nicer" number (e.g. 1, 2, 5, 10, 20, 50 ...)
    const stepExponent = Math.floor(Math.log10(step));
    const stepSignificand = step / Math.pow(10, stepExponent);

    if (stepSignificand > 5) step = 10 * Math.pow(10, stepExponent);
    else if (stepSignificand > 2) step = 5 * Math.pow(10, stepExponent);
    else if (stepSignificand > 1) step = 2 * Math.pow(10, stepExponent);
    else step = 1 * Math.pow(10, stepExponent);

    return Math.max(1, step); // Ensure step is at least 1 for whole numbers
}

function createAxisVisuals() {
    if (axisVisualsGroup) {
        while (axisVisualsGroup.children.length > 0) {
            const child = axisVisualsGroup.children[0];
            axisVisualsGroup.remove(child);
            if (child.geometry) child.geometry.dispose();
            if (child.material) {
                if (child.material.map) child.material.map.dispose();
                child.material.dispose();
            }
        }
        scene.remove(axisVisualsGroup);
    }
    axisVisualsGroup = new THREE.Group();
    axisVisualsGroup.name = "AxisVisuals";

    const defaultVisualExtent = 5; // Minimum visual length for an axis segment from origin if data is small

    // --- Determine Visual Start/End for Each Axis Line and Draw Them ---
    const xAxisMat = new THREE.LineBasicMaterial({ color: 0xff0000 });
    const yAxisMat = new THREE.LineBasicMaterial({ color: 0x008f00 });
    const zAxisMat = new THREE.LineBasicMaterial({ color: 0x0000ff });

    let visualStartX, visualEndX;
    if (sceneMinBounds.x === 0 && sceneMaxBounds.x === 0) { // Data is exactly at X=0
        visualStartX = -defaultVisualExtent;
        visualEndX = defaultVisualExtent;
    } else {
        visualStartX = (sceneMinBounds.x < 0) ? -Math.max(Math.abs(sceneMinBounds.x) * MIN_AXIS_LENGTH_FACTOR, defaultVisualExtent) : 0;
        visualEndX = (sceneMaxBounds.x > 0) ? Math.max(sceneMaxBounds.x * MIN_AXIS_LENGTH_FACTOR, defaultVisualExtent) : 0;
        // Ensure at least defaultVisualExtent if data is one-sided and small
        if (sceneMinBounds.x >= 0 && sceneMaxBounds.x > 0 && visualEndX < defaultVisualExtent) visualEndX = defaultVisualExtent;
        if (sceneMaxBounds.x <= 0 && sceneMinBounds.x < 0 && Math.abs(visualStartX) < defaultVisualExtent) visualStartX = -defaultVisualExtent;
    }
    if (visualStartX !== visualEndX) { // Only draw if there's a length
         axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(visualStartX, 0, 0), new THREE.Vector3(visualEndX, 0, 0)]), xAxisMat));
    }


    let visualStartY, visualEndY;
    if (sceneMinBounds.y === 0 && sceneMaxBounds.y === 0) {
        visualStartY = -defaultVisualExtent;
        visualEndY = defaultVisualExtent;
    } else {
        visualStartY = (sceneMinBounds.y < 0) ? -Math.max(Math.abs(sceneMinBounds.y) * MIN_AXIS_LENGTH_FACTOR, defaultVisualExtent) : 0;
        visualEndY = (sceneMaxBounds.y > 0) ? Math.max(sceneMaxBounds.y * MIN_AXIS_LENGTH_FACTOR, defaultVisualExtent) : 0;
        if (sceneMinBounds.y >= 0 && sceneMaxBounds.y > 0 && visualEndY < defaultVisualExtent) visualEndY = defaultVisualExtent;
        if (sceneMaxBounds.y <= 0 && sceneMinBounds.y < 0 && Math.abs(visualStartY) < defaultVisualExtent) visualStartY = -defaultVisualExtent;
    }
    if (visualStartY !== visualEndY) {
        axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, visualStartY, 0), new THREE.Vector3(0, visualEndY, 0)]), yAxisMat));
    }

    let visualStartZ, visualEndZ;
    if (sceneMinBounds.z === 0 && sceneMaxBounds.z === 0) {
        visualStartZ = -defaultVisualExtent;
        visualEndZ = defaultVisualExtent;
    } else {
        visualStartZ = (sceneMinBounds.z < 0) ? -Math.max(Math.abs(sceneMinBounds.z) * MIN_AXIS_LENGTH_FACTOR, defaultVisualExtent) : 0;
        visualEndZ = (sceneMaxBounds.z > 0) ? Math.max(sceneMaxBounds.z * MIN_AXIS_LENGTH_FACTOR, defaultVisualExtent) : 0;
        if (sceneMinBounds.z >= 0 && sceneMaxBounds.z > 0 && visualEndZ < defaultVisualExtent) visualEndZ = defaultVisualExtent;
        if (sceneMaxBounds.z <= 0 && sceneMinBounds.z < 0 && Math.abs(visualStartZ) < defaultVisualExtent) visualStartZ = -defaultVisualExtent;
    }
    if (visualStartZ !== visualEndZ) {
        axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, 0, visualStartZ), new THREE.Vector3(0, 0, visualEndZ)]), zAxisMat));
    }

    // --- Ticks and Labels ---
    // Use the largest absolute visual extent for determining a common tick step
    const maxVisualExtentForStep = Math.max(
        Math.abs(visualStartX), Math.abs(visualEndX),
        Math.abs(visualStartY), Math.abs(visualEndY),
        Math.abs(visualStartZ), Math.abs(visualEndZ),
        defaultVisualExtent // Ensure a minimum step basis if all extents are tiny
    );
    const tickStep = getNiceTickStep(maxVisualExtentForStep / TARGET_TICKS_PER_AXIS_SIDE);
    const scaledTickLineLength = AXIS_TICK_LINE_BASE_LENGTH * (tickStep / 5); // Scale based on tickStep
    const tickLabelOffsetFromLine = scaledTickLineLength * 1.5;
    const tickLineMaterial = new THREE.LineBasicMaterial({ color: AXIS_TICK_LINE_COLOR });

    function addTicks(axisChar, visualMin, visualMax, color) {
        if (tickStep === 0 || visualMin === visualMax) return; // No ticks if no step or no length

        // Iterate from visualMin up to visualMax, generating ticks
        // Start iteration from the first multiple of tickStep at or after visualMin
        // or at or before visualMax for the negative direction.

        // Ticks from 0 towards visualMax (positive or less negative direction)
        for (let val = 0; ; val += tickStep) {
            if (val > visualMax && val !== 0) break; // Stop if beyond visualMax, unless val is 0 and visualMax is negative
            if (val < visualMin && val !== 0) { // If 0 is beyond visualMin, start from next tick
                 if (val + tickStep > visualMin) { /* allow if next step is in range */ } else {
                    if (tickStep === 0) break;
                    continue;
                 }
            }


            const tickPos = new THREE.Vector3();
            tickPos[axisChar] = val;

            // Tick Lines
            if (axisChar === 'x') {
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(val, -scaledTickLineLength/2, 0), new THREE.Vector3(val, scaledTickLineLength/2, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(val, 0, -scaledTickLineLength/2), new THREE.Vector3(val, 0, scaledTickLineLength/2)]), tickLineMaterial));
            } else if (axisChar === 'y') {
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, val, 0), new THREE.Vector3(scaledTickLineLength/2, val, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, val, -scaledTickLineLength/2), new THREE.Vector3(0, val, scaledTickLineLength/2)]), tickLineMaterial));
            } else {
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, 0, val), new THREE.Vector3(scaledTickLineLength/2, 0, val)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, -scaledTickLineLength/2, val), new THREE.Vector3(0, scaledTickLineLength/2, val)]), tickLineMaterial));
            }

            const labelPos = tickPos.clone();
            if (axisChar === 'x') { labelPos.y -= tickLabelOffsetFromLine; labelPos.z -= tickLabelOffsetFromLine; }
            else if (axisChar === 'y') { labelPos.x -= tickLabelOffsetFromLine; labelPos.z -= tickLabelOffsetFromLine; }
            else { labelPos.x -= tickLabelOffsetFromLine; labelPos.y -= tickLabelOffsetFromLine; }

            if (val === 0 && axisChar !== 'x') { /* Only X draws '0' */ }
            else {
                axisVisualsGroup.add(createTextLabel(String(val), labelPos, AXIS_TICK_LABEL_FONT_SIZE, AXIS_TICK_LABEL_COLOR, null, 1.0, false));
            }
            if (val === 0 && visualMax === 0 && visualMin === 0) break; // Only origin point
            if (val === visualMax) break; // Reached the end
            if (tickStep === 0) break;
            if (val > visualMax && val !== 0) break; // Ensure termination if overshoot
        }

        // Ticks from -tickStep towards visualMin (negative direction)
        for (let val = -tickStep; ; val -= tickStep) {
            if (val < visualMin && val !== 0 ) break; // Stop if beyond visualMin
            if (val > visualMax && val !== 0) { // If -0 is beyond visualMax (e.g. visualMax is -10)
                if (val - tickStep < visualMax) { /* allow */ } else {
                    if (tickStep === 0) break;
                    continue;
                }
            }

            const tickPos = new THREE.Vector3();
            tickPos[axisChar] = val;
            // Tick Lines
             if (axisChar === 'x') {
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(val, -scaledTickLineLength/2, 0), new THREE.Vector3(val, scaledTickLineLength/2, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(val, 0, -scaledTickLineLength/2), new THREE.Vector3(val, 0, scaledTickLineLength/2)]), tickLineMaterial));
            } else if (axisChar === 'y') {
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, val, 0), new THREE.Vector3(scaledTickLineLength/2, val, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, val, -scaledTickLineLength/2), new THREE.Vector3(0, val, scaledTickLineLength/2)]), tickLineMaterial));
            } else {
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, 0, val), new THREE.Vector3(scaledTickLineLength/2, 0, val)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, -scaledTickLineLength/2, val), new THREE.Vector3(0, scaledTickLineLength/2, val)]), tickLineMaterial));
            }

            const labelPos = tickPos.clone();
            if (axisChar === 'x') { labelPos.y -= tickLabelOffsetFromLine; labelPos.z -= tickLabelOffsetFromLine; }
            else if (axisChar === 'y') { labelPos.x -= tickLabelOffsetFromLine; labelPos.z -= tickLabelOffsetFromLine; }
            else { labelPos.x -= tickLabelOffsetFromLine; labelPos.y -= tickLabelOffsetFromLine; }
            axisVisualsGroup.add(createTextLabel(String(val), labelPos, AXIS_TICK_LABEL_FONT_SIZE, AXIS_TICK_LABEL_COLOR, null, 1.0, false));

            if (val === visualMin) break; // Reached the end
            if (tickStep === 0) break;
            if (val < visualMin && val !==0) break; // Ensure termination
        }

        // Axis Name Label (X, Y, Z)
        const axisNameLabelPos = new THREE.Vector3();
        // Position name label at the positive extent of THIS axis's *actual drawn line* (visualMax)
        // If visualMax is 0 (e.g. data only negative), use defaultVisualExtent for label position
        const labelEndPos = (visualMax === 0 && visualMin < 0) ? defaultVisualExtent : visualMax;
        axisNameLabelPos[axisChar] = labelEndPos + tickLabelOffsetFromLine * 1.5;
        axisVisualsGroup.add(createTextLabel(axisChar.toUpperCase(), axisNameLabelPos, AXIS_TICK_LABEL_FONT_SIZE * 1.2, color, null, 1.2, true));
    }

    addTicks('x', visualStartX, visualEndX, new THREE.Color(0xff0000));
    addTicks('y', visualStartY, visualEndY, new THREE.Color(0x00ff00));
    addTicks('z', visualStartZ, visualEndZ, new THREE.Color(0x0000ff));

    scene.add(axisVisualsGroup);
}


// --- Slice Logic ---
function updateSlice() {
    const axis = sliceAxisSelect.value;
    const coord = parseFloat(sliceCoordinateSlider.value);
    sliceCoordValueSpan.textContent = coord.toFixed(2);
    const do2DProjection = show2DProjectionCheckbox.checked && axis !== 'none';
    clearProjectionVisuals();
    allVisualsGroup.children.forEach(visualGroup => {
        const boxData = visualGroup.userData.boxData; if (!boxData) return;
        let intersects = false;
        if (axis !== 'none') {
            if (axis === 'x') intersects = (boxData.minBounds.x <= coord && boxData.maxBounds.x >= coord);
            else if (axis === 'y') intersects = (boxData.minBounds.y <= coord && boxData.maxBounds.y >= coord);
            else if (axis === 'z') intersects = (boxData.minBounds.z <= coord && boxData.maxBounds.z >= coord);
        }
        if (axis === 'none') visualGroup.visible = true;
        else {
            if (do2DProjection) {
                visualGroup.visible = false;
                if (intersects) create2DProjection(boxData, axis, coord);
            } else visualGroup.visible = intersects;
        }
    });
    updateSlicePlaneHelper(axis, coord);
    projectionVisualsGroup.visible = do2DProjection;
}
function clearProjectionVisuals() {
    while (projectionVisualsGroup.children.length > 0) {
        const child = projectionVisualsGroup.children[0]; projectionVisualsGroup.remove(child);
        if (child.geometry) child.geometry.dispose();
        if (child.material) { if (child.material.map) child.material.map.dispose(); child.material.dispose(); }
    }
}
function create2DProjection(boxData, axis, sliceCoord) {
    let projMin = new THREE.Vector2(), projMax = new THREE.Vector2();
    let textPos3D = new THREE.Vector3(), outlinePoints = [];
    if (axis === 'x') {
        projMin.set(boxData.minBounds.y, boxData.minBounds.z); projMax.set(boxData.maxBounds.y, boxData.maxBounds.z);
        outlinePoints = [ new THREE.Vector3(sliceCoord, projMin.x, projMin.y), new THREE.Vector3(sliceCoord, projMax.x, projMin.y), new THREE.Vector3(sliceCoord, projMax.x, projMax.y), new THREE.Vector3(sliceCoord, projMin.x, projMax.y) ];
        textPos3D.set(sliceCoord, (projMin.x + projMax.x) / 2, (projMin.y + projMax.y) / 2);
    } else if (axis === 'y') {
        projMin.set(boxData.minBounds.x, boxData.minBounds.z); projMax.set(boxData.maxBounds.x, boxData.maxBounds.z);
        outlinePoints = [ new THREE.Vector3(projMin.x, sliceCoord, projMin.y), new THREE.Vector3(projMax.x, sliceCoord, projMin.y), new THREE.Vector3(projMax.x, sliceCoord, projMax.y), new THREE.Vector3(projMin.x, sliceCoord, projMax.y) ];
        textPos3D.set((projMin.x + projMax.x) / 2, sliceCoord, (projMin.y + projMax.y) / 2);
    } else if (axis === 'z') {
        projMin.set(boxData.minBounds.x, boxData.minBounds.y); projMax.set(boxData.maxBounds.x, boxData.maxBounds.y);
        outlinePoints = [ new THREE.Vector3(projMin.x, projMin.y, sliceCoord), new THREE.Vector3(projMax.x, projMin.y, sliceCoord), new THREE.Vector3(projMax.x, projMax.y, sliceCoord), new THREE.Vector3(projMin.x, projMax.y, sliceCoord) ];
        textPos3D.set((projMin.x + projMax.x) / 2, (projMin.y + projMax.y) / 2, sliceCoord);
    }
    const w = projMax.x - projMin.x, h = projMax.y - projMin.y; if (w <= 0 || h <= 0) return;
    const outlineGeom = new THREE.BufferGeometry().setFromPoints(outlinePoints);
    const outlineMat = new THREE.LineBasicMaterial({ color: PROJECTION_OUTLINE_COLOR, linewidth: 2, transparent: true, opacity: 0.8 });
    const outline = new THREE.LineLoop(outlineGeom, outlineMat);
    outline.position[axis] += SLICE_PLANE_VISIBILITY_OFFSET; projectionVisualsGroup.add(outline);
    const labelPos = textPos3D.clone(); labelPos[axis] += SLICE_PLANE_VISIBILITY_OFFSET * 2;
    const projLabel = createTextLabel(boxData.text, labelPos, PROJECTION_LABEL_FONT_SIZE, PROJECTION_TEXT_COLOR, PROJECTION_TEXT_BG_COLOR, 0.8);
    projLabel.renderOrder = 1; projectionVisualsGroup.add(projLabel);
}
function updateSlicePlaneHelper(axis, coord) {
    let isHelperVisible = showSlicePlaneCheckbox.checked && axis !== 'none';
    if (show2DProjectionCheckbox.checked && axis !== 'none') isHelperVisible = true;
    slicePlaneHelper.visible = isHelperVisible; if (!isHelperVisible) return;
    const overallSize = new THREE.Vector3(); new THREE.Box3(sceneMinBounds, sceneMaxBounds).getSize(overallSize);
    const overallCenter = new THREE.Vector3(); new THREE.Box3(sceneMinBounds, sceneMaxBounds).getCenter(overallCenter);
    slicePlaneHelper.rotation.set(0,0,0); slicePlaneHelper.position.copy(overallCenter);
    if (slicePlaneHelper.geometry) slicePlaneHelper.geometry.dispose();
    let pw = 1, ph = 1;
    if (axis === 'x') {
        pw = Math.max(1, overallSize.y); ph = Math.max(1, overallSize.z);
        slicePlaneHelper.geometry = new THREE.PlaneGeometry(pw * 1.05, ph * 1.05);
        slicePlaneHelper.rotation.y = Math.PI / 2; slicePlaneHelper.position.set(coord, overallCenter.y, overallCenter.z);
    } else if (axis === 'y') {
        pw = Math.max(1, overallSize.x); ph = Math.max(1, overallSize.z);
        slicePlaneHelper.geometry = new THREE.PlaneGeometry(pw * 1.05, ph * 1.05);
        slicePlaneHelper.rotation.x = Math.PI / 2; slicePlaneHelper.position.set(overallCenter.x, coord, overallCenter.z);
    } else if (axis === 'z') {
        pw = Math.max(1, overallSize.x); ph = Math.max(1, overallSize.y);
        slicePlaneHelper.geometry = new THREE.PlaneGeometry(pw * 1.05, ph * 1.05);
        slicePlaneHelper.position.set(overallCenter.x, overallCenter.y, coord);
    }
}
function updateSliderRange() {
    const axis = sliceAxisSelect.value; let minVal = 0, maxVal = 0, step = 0.1;
    if (dataBoxes.length > 0 && axis !== 'none') {
        if (axis === 'x') { minVal = sceneMinBounds.x; maxVal = sceneMaxBounds.x; }
        else if (axis === 'y') { minVal = sceneMinBounds.y; maxVal = sceneMaxBounds.y; }
        else if (axis === 'z') { minVal = sceneMinBounds.z; maxVal = sceneMaxBounds.z; }
        step = (maxVal - minVal === 0) ? 0.1 : Math.max(0.001, (maxVal - minVal) / 200);
    } else { minVal = 0; maxVal = 1; step = 0.1; }
    sliceCoordinateSlider.min = minVal; sliceCoordinateSlider.max = maxVal; sliceCoordinateSlider.step = step;
    if (axis === 'none' || (maxVal === minVal && minVal === 0 && maxVal === 0 )) {
        sliceCoordinateSlider.disabled = true; sliceCoordinateSlider.value = minVal;
    } else {
        sliceCoordinateSlider.disabled = false; const currentVal = parseFloat(sliceCoordinateSlider.value);
        if (isNaN(currentVal) || currentVal < minVal || currentVal > maxVal) { sliceCoordinateSlider.value = minVal; }
    }
    sliceCoordValueSpan.textContent = parseFloat(sliceCoordinateSlider.value).toFixed(2);
}

// --- Mouse Dragging Logic ---
function onDocumentMouseDown(event) {
    event.preventDefault(); mouse.x = (event.clientX / window.innerWidth) * 2 - 1; mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;
    raycaster.setFromCamera(mouse, camera); const currentSliceAxis = sliceAxisSelect.value;
    if (currentSliceAxis === 'none' || !slicePlaneHelper.visible) return;
    const intersects = raycaster.intersectObject(slicePlaneHelper, false);
    if (intersects.length > 0) {
        isDraggingSlicePlane = true; controls.enabled = false; renderer.domElement.style.cursor = 'grabbing';
        dragActiveAxis = currentSliceAxis; dragInitialCoordinate = parseFloat(sliceCoordinateSlider.value);
        dragStartPointOnHelper.copy(intersects[0].point);
        camera.getWorldDirection(dragPlane.normal); dragPlane.setFromNormalAndCoplanarPoint(dragPlane.normal, dragStartPointOnHelper);
    }
}
function onDocumentMouseMove(event) {
    event.preventDefault(); mouse.x = (event.clientX / window.innerWidth) * 2 - 1; mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;
    if (!isDraggingSlicePlane && sliceAxisSelect.value !== 'none' && slicePlaneHelper.visible) {
        raycaster.setFromCamera(mouse, camera); const intersects = raycaster.intersectObject(slicePlaneHelper, false);
        renderer.domElement.style.cursor = intersects.length > 0 ? 'grab' : 'default';
    }
    if (!isDraggingSlicePlane) return;
    raycaster.setFromCamera(mouse, camera);
    if (raycaster.ray.intersectPlane(dragPlane, planeIntersectPoint)) {
        const dragVector = planeIntersectPoint.clone().sub(dragStartPointOnHelper);
        let coordChange = 0;
        if (dragActiveAxis === 'x') coordChange = dragVector.x; else if (dragActiveAxis === 'y') coordChange = dragVector.y; else if (dragActiveAxis === 'z') coordChange = dragVector.z;
        let newCoord = dragInitialCoordinate + coordChange;
        let min = 0, max = 0;
        if (dragActiveAxis === 'x') { min = sceneMinBounds.x; max = sceneMaxBounds.x; }
        else if (dragActiveAxis === 'y') { min = sceneMinBounds.y; max = sceneMaxBounds.y; }
        else if (dragActiveAxis === 'z') { min = sceneMinBounds.z; max = sceneMaxBounds.z; }
        newCoord = Math.max(min, Math.min(max, newCoord));
        if (Math.abs(parseFloat(sliceCoordinateSlider.value) - newCoord) > (parseFloat(sliceCoordinateSlider.step) / 2) || sliceCoordinateSlider.value == min || sliceCoordinateSlider.value == max) {
            sliceCoordinateSlider.value = newCoord.toFixed(Math.max(2, (sliceCoordinateSlider.step.split('.')[1] || '').length));
            updateSlice();
        }
    }
}
function onDocumentMouseUp(event) {
    if (isDraggingSlicePlane) {
        isDraggingSlicePlane = false; controls.enabled = true;
        renderer.domElement.style.cursor = (sliceAxisSelect.value !== 'none' && slicePlaneHelper.visible) ? 'grab' : 'default';
    }
}

// --- Animation Loop & Resize ---
function animate() { requestAnimationFrame(animate); controls.update(); renderer.render(scene, camera); }
function onWindowResize() {
    camera.aspect = window.innerWidth / window.innerHeight; camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
}

// Initial call
updateSliderRange();
updateSlice();