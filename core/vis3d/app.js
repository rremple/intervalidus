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
    }
} catch (error) {
    console.error("Error processing title URL parameter:", error);
}

let scene, camera, renderer, controls;
let allVisualsGroup;
let slicePlaneHelper;
let dataBoxes = [];
let sceneMinBounds, sceneMaxBounds, sceneSize;
let projectionVisualsGroup;
let axisVisualsGroup;
let sliceAxisIntersectionDot;

// --- UI Elements ---
const sliceAxisSelect = document.getElementById('sliceAxis');
const sliceCoordinateSlider = document.getElementById('sliceCoordinate');
const sliceCoordValueSpan = document.getElementById('sliceCoordValue');
const showSlicePlaneCheckbox = document.getElementById('showSlicePlane');
const show2DProjectionCheckbox = document.getElementById('show2DProjection');

// --- Constants ---
const LABEL_FONT_SIZE = 20;
const PROJECTION_LABEL_FONT_SIZE = 18;
const AXIS_TICK_LABEL_FONT_SIZE = 14;
const AXIS_TICK_LABEL_COLOR = 'lightgray';
const LINE_SPACING_FACTOR = 0.3;
const LABEL_TEXT_COLOR = 'white';
const LABEL_BG_COLOR = 'rgba(0, 0, 0, 0.6)';
const BOX_HELPER_COLOR = 0x00ff00;
const PROJECTION_OUTLINE_COLOR = 0xffa500;
const PROJECTION_TEXT_COLOR = 'black';
const PROJECTION_TEXT_BG_COLOR = 'rgba(255, 165, 0, 0.7)';
const SLICE_PLANE_VISIBILITY_OFFSET = 0.005;
const AXIS_LABEL_OFFSET = 0.5;
const AXIS_TICK_LINE_BASE_LENGTH = 0.2;
const AXIS_TICK_LINE_COLOR = 0xaaaaaa;

const MIN_AXIS_LENGTH_FACTOR = 1.2;
const TARGET_TICKS_PER_AXIS_SIDE = 4;
const SLICE_AXIS_DOT_SIZE = 1.0; // Relative to overall scene scale, adjust as needed
const SLICE_AXIS_DOT_COLOR = 0xffffff; // White dot

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

    defineDataBoxes();

    const center = new THREE.Vector3();
    new THREE.Box3(sceneMinBounds, sceneMaxBounds).getCenter(center);
    const maxDim = Math.max(sceneSize.x, sceneSize.y, sceneSize.z, 1);

    camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, Math.max(2000, maxDim * 5));
    camera.position.set(center.x + maxDim * 1.1, center.y + maxDim * 1.1, center.z + maxDim * 1.8);
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

    // --- Create Slice Axis Intersection Dot ---
    const dotGeometry = new THREE.SphereGeometry(SLICE_AXIS_DOT_SIZE, 16, 8); // Small sphere
    const dotMaterial = new THREE.MeshBasicMaterial({ color: SLICE_AXIS_DOT_COLOR });
    sliceAxisIntersectionDot = new THREE.Mesh(dotGeometry, dotMaterial);
    sliceAxisIntersectionDot.visible = false; // Initially hidden
    scene.add(sliceAxisIntersectionDot);
    // --- End Dot Creation ---

    createAxisVisuals();

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
                        (typeof item.text === 'string' || typeof item.text1 === 'string')
                    ) {
                        validatedBoxes.push({
                            minBounds: new THREE.Vector3().fromArray(item.min),
                            maxBounds: new THREE.Vector3().fromArray(item.max),
                            text1: item.text1 || item.text || "",
                            text2: item.text2 || ""
                        });
                    } else { console.warn("Invalid item in URL data:", item); }
                }
                if (validatedBoxes.length > 0) {
                    dataBoxes = validatedBoxes; customDataProvided = true; console.log("Loaded data from URL.");
                } else { console.warn("URL data valid JSON but no valid boxes. Using default."); }
            } else { console.warn("URL data not valid array. Using default."); }
        }
    } catch (error) { console.error("Error parsing URL data:", error, ". Using default data."); }

    if (!customDataProvided) {
        //console.log("Using default data set.");
        dataBoxes = [
            { minBounds: new THREE.Vector3(0,0,0), maxBounds: new THREE.Vector3(5,5,5), text1: "Hello gyp", text2: "Main Zone" },
            { minBounds: new THREE.Vector3(5,0,0), maxBounds: new THREE.Vector3(10,5,5), text1: "World qjp", text2: "(Part A)" },
            { minBounds: new THREE.Vector3(2,6,2), maxBounds: new THREE.Vector3(8,10,8), text1: "3D Data XYZ" },
            { minBounds: new THREE.Vector3(-5,-5,-2), maxBounds: new THREE.Vector3(0,0,3), text1: "Zone Alpha", text2: "ID: ZA-001" },
            { minBounds: new THREE.Vector3(6,-4,1), maxBounds: new THREE.Vector3(9,-1,4), text1: "Region Beta"},
            { minBounds: new THREE.Vector3(3,3,-3), maxBounds: new THREE.Vector3(7,7,2), text1: "Slice Me!", text2: "Layer 1" }
        ];
    }

    sceneMinBounds = new THREE.Vector3(Infinity,Infinity,Infinity);
    sceneMaxBounds = new THREE.Vector3(-Infinity,-Infinity,-Infinity);
    if (dataBoxes.length > 0) {
        dataBoxes.forEach(box => { sceneMinBounds.min(box.minBounds); sceneMaxBounds.max(box.maxBounds); });
    } else {
        console.warn("No data boxes. Setting default bounds.");
        sceneMinBounds.set(-1,-1,-1); sceneMaxBounds.set(1,1,1);
        dataBoxes.push({ minBounds: new THREE.Vector3(-0.5,-0.5,-0.5), maxBounds: new THREE.Vector3(0.5,0.5,0.5), text1: "No Data", text2:"" });
        sceneMinBounds.min(dataBoxes[0].minBounds); sceneMaxBounds.max(dataBoxes[0].maxBounds);
    }
    // Calculate overall size of the data bounding box
    sceneSize = new THREE.Vector3().subVectors(sceneMaxBounds, sceneMinBounds);
}

// --- Visualization Creation ---
function createTextLabel(
    textLine1, textLine2, position,
    fontSize = LABEL_FONT_SIZE, textColor = LABEL_TEXT_COLOR, bgColor = null,
    textScaleFactor = 1.0, bold = true
) {
    const canvas = document.createElement('canvas'); const context = canvas.getContext('2d');
    const fontWeight = bold ? 'Bold ' : '';
    context.font = `${fontWeight}${fontSize}px Arial`;

    const lines = [];
    if (textLine1 && String(textLine1).trim() !== "") lines.push(String(textLine1).trim()); // Ensure string conversion
    if (textLine2 && String(textLine2).trim() !== "") lines.push(String(textLine2).trim());
    if (lines.length === 0) lines.push(" "); // Default to a space if no text

    let maxWidth = 0;
    lines.forEach(line => {
        const metrics = context.measureText(line);
        if (metrics.width > maxWidth) maxWidth = metrics.width;
    });

    const lineHeight = fontSize;
    const lineSpacing = lineHeight * LINE_SPACING_FACTOR;
    const totalTextHeight = (lines.length * lineHeight) + (Math.max(0, lines.length - 1) * lineSpacing);
    const verticalPadding = fontSize * 0.4;
    const horizontalPadding = fontSize * (bgColor ? 0.3 : 0.05);

    canvas.width = Math.ceil(maxWidth + horizontalPadding * 2);
    canvas.height = Math.ceil(totalTextHeight + verticalPadding * 2);

    if (bgColor) { // Only draw background if bgColor is provided
        context.fillStyle = bgColor; context.beginPath(); const r = fontSize / 2.5;
        context.moveTo(r, 0); context.lineTo(canvas.width - r, 0); context.quadraticCurveTo(canvas.width, 0, canvas.width, r);
        context.lineTo(canvas.width, canvas.height - r); context.quadraticCurveTo(canvas.width, canvas.height, canvas.width - r, canvas.height);
        context.lineTo(r, canvas.height); context.quadraticCurveTo(0, canvas.height, 0, canvas.height - r);
        context.lineTo(0, r); context.quadraticCurveTo(0, 0, r, 0); context.closePath(); context.fill();
    }

    context.font = `${fontWeight}${fontSize}px Arial`;
    context.fillStyle = textColor;
    context.textAlign = 'center';
    context.textBaseline = 'middle'; // Crucial for vertical centering

    let startY = canvas.height / 2 - totalTextHeight / 2 + lineHeight / 2; // Center the whole block

    lines.forEach((line, index) => {
        const yPos = startY + index * (lineHeight + lineSpacing);
        context.fillText(line, canvas.width / 2, yPos);
    });

    const texture = new THREE.CanvasTexture(canvas); texture.needsUpdate = true;
    const material = new THREE.SpriteMaterial({ map: texture, transparent: true, depthTest: true, depthWrite: true, sizeAttenuation: true });
    const sprite = new THREE.Sprite(material);
    // Adjust sprite height scale: base on font size, scale factor, and slightly more if two lines
    const baseSpriteHeight = 0.6 * (fontSize / LABEL_FONT_SIZE);
    const multiLineFactor = lines.length > 1 ? 1.4 : 1.0; // Increase height more for 2 lines
    const spriteH = baseSpriteHeight * textScaleFactor * multiLineFactor;

    sprite.scale.set((spriteH * canvas.width) / canvas.height, spriteH, 1);
    sprite.position.copy(position);
    return sprite;
}

function createBoxVisual(boxData) {
    const group = new THREE.Group(); group.userData.boxData = boxData;
    const box3 = new THREE.Box3(boxData.minBounds, boxData.maxBounds);
    const boxHelper = new THREE.Box3Helper(box3, BOX_HELPER_COLOR); group.add(boxHelper);
    const center = new THREE.Vector3(); box3.getCenter(center);
    const label = createTextLabel(
        boxData.text1, boxData.text2, center,
        LABEL_FONT_SIZE, LABEL_TEXT_COLOR, LABEL_BG_COLOR, 1.0, true
    );
    group.add(label); return group;
}

// Helper function to calculate "nice" step for axis ticks
function getNiceTickStep(maxValue) {
    if (maxValue <= 0) return 1; // Handle non-positive max value gracefully
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
    const stepExponent = Math.floor(Math.log10(Math.max(step, 0.00001))); // Avoid log(0)
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
            const child = axisVisualsGroup.children[0]; axisVisualsGroup.remove(child);
            if (child.geometry) child.geometry.dispose();
            if (child.material) { if (child.material.map) child.material.map.dispose(); child.material.dispose(); }
        }
        scene.remove(axisVisualsGroup);
    }
    axisVisualsGroup = new THREE.Group(); axisVisualsGroup.name = "AxisVisuals";

    const defaultVisualExtent = 5; // Minimum visual length for an axis segment from origin if data is small

    // --- Determine Visual Start/End for Each Axis Line and Draw Them ---
    const xAxisMat = new THREE.LineBasicMaterial({ color: 0xff0000 });
    const yAxisMat = new THREE.LineBasicMaterial({ color: 0x008f00 });
    const zAxisMat = new THREE.LineBasicMaterial({ color: 0x0000ff });

    function calculateVisualRange(dataMin, dataMax) {
        let visualMin = 0, visualMax = 0;
        if (dataMin === 0 && dataMax === 0) {
            visualMin = -defaultVisualExtent; visualMax = defaultVisualExtent;
        } else {
            visualMin = (dataMin < 0) ? -Math.max(Math.abs(dataMin) * MIN_AXIS_LENGTH_FACTOR, defaultVisualExtent) : 0;
            visualMax = (dataMax > 0) ? Math.max(dataMax * MIN_AXIS_LENGTH_FACTOR, defaultVisualExtent) : 0;
            if (dataMin >= 0 && dataMax > 0 && visualMax < defaultVisualExtent && visualMin === 0) visualMax = defaultVisualExtent;
            if (dataMax <= 0 && dataMin < 0 && Math.abs(visualMin) < defaultVisualExtent && visualMax === 0) visualMin = -defaultVisualExtent;
            // If one side is 0 and the other is not, ensure the 0 side remains 0 unless data is exactly at 0
            if (dataMin === 0 && dataMax > 0) visualMin = 0;
            if (dataMax === 0 && dataMin < 0) visualMax = 0;
        }
        return { visualMin, visualMax };
    }

    const rangeX = calculateVisualRange(sceneMinBounds.x, sceneMaxBounds.x);
    if (rangeX.visualMin !== rangeX.visualMax) axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(rangeX.visualMin,0,0), new THREE.Vector3(rangeX.visualMax,0,0)]), xAxisMat));

    const rangeY = calculateVisualRange(sceneMinBounds.y, sceneMaxBounds.y);
    if (rangeY.visualMin !== rangeY.visualMax) axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0,rangeY.visualMin,0), new THREE.Vector3(0,rangeY.visualMax,0)]), yAxisMat));

    const rangeZ = calculateVisualRange(sceneMinBounds.z, sceneMaxBounds.z);
    if (rangeZ.visualMin !== rangeZ.visualMax) axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0,0,rangeZ.visualMin), new THREE.Vector3(0,0,rangeZ.visualMax)]), zAxisMat));


    const maxVisualExtentForStep = Math.max(
        Math.abs(rangeX.visualMin), Math.abs(rangeX.visualMax),
        Math.abs(rangeY.visualMin), Math.abs(rangeY.visualMax),
        Math.abs(rangeZ.visualMin), Math.abs(rangeZ.visualMax),
        defaultVisualExtent // Ensure a minimum step basis if all extents are tiny
    );
    const tickStep = getNiceTickStep(maxVisualExtentForStep / TARGET_TICKS_PER_AXIS_SIDE);
    const scaledTickLineLength = AXIS_TICK_LINE_BASE_LENGTH * (tickStep / 5); // Scale based on tickStep
    const tickLabelOffsetFromLine = scaledTickLineLength * 1.5;
    const tickLineMaterial = new THREE.LineBasicMaterial({ color: AXIS_TICK_LINE_COLOR });

    function addTicks(axisChar, visualMin, visualMax, color) {
        if (tickStep === 0 || visualMin === visualMax) return; // No ticks if no step or no length

        // Positive Ticks (including 0 if in range [visualMin, visualMax])
        for (let val = 0; ; val += tickStep) {
            if (val > visualMax + tickStep * 0.01) break; // Stop if clearly beyond visualMax
            if (val < visualMin - tickStep * 0.01 && val !== 0) { // If 0 is beyond visualMin, this val is not needed
                 if (tickStep === 0) break; continue;
            }

            // Draw tick and label for val
            const tickPos = new THREE.Vector3(); tickPos[axisChar] = val;
            if (axisChar === 'x') { /* ... tick lines ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(val, -scaledTickLineLength/2, 0), new THREE.Vector3(val, scaledTickLineLength/2, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(val, 0, -scaledTickLineLength/2), new THREE.Vector3(val, 0, scaledTickLineLength/2)]), tickLineMaterial));
            } else if (axisChar === 'y') { /* ... tick lines ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, val, 0), new THREE.Vector3(scaledTickLineLength/2, val, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, val, -scaledTickLineLength/2), new THREE.Vector3(0, val, scaledTickLineLength/2)]), tickLineMaterial));
            } else { /* ... tick lines ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, 0, val), new THREE.Vector3(scaledTickLineLength/2, 0, val)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, -scaledTickLineLength/2, val), new THREE.Vector3(0, scaledTickLineLength/2, val)]), tickLineMaterial));
            }

            const labelPos = tickPos.clone();
            if (axisChar === 'x') { labelPos.y -= tickLabelOffsetFromLine; labelPos.z -= tickLabelOffsetFromLine; }
            else if (axisChar === 'y') { labelPos.x -= tickLabelOffsetFromLine; labelPos.z -= tickLabelOffsetFromLine; }
            else { labelPos.x -= tickLabelOffsetFromLine; labelPos.y -= tickLabelOffsetFromLine; }
            if (val === 0 && axisChar !== 'x') {}
            else axisVisualsGroup.add(createTextLabel(String(val), null, labelPos, AXIS_TICK_LABEL_FONT_SIZE, AXIS_TICK_LABEL_COLOR, null, 1.0, false));

            if (val === 0 && visualMax === 0 && visualMin === 0) break;
            if (val >= visualMax && val !==0) break; // Stop if we've reached or passed visualMax (and not at origin)
             if (tickStep === 0) break;
        }

        // Negative Ticks (excluding 0, as it's handled by positive loop)
        for (let val = -tickStep; ; val -= tickStep) {
            if (val < visualMin - tickStep * 0.01) break; // Stop if clearly beyond visualMin
            if (val > visualMax + tickStep * 0.01) { // If -tickStep is beyond visualMax (e.g. visualMax is very negative)
                 if (tickStep === 0) break; continue;
            }

            const tickPos = new THREE.Vector3(); tickPos[axisChar] = val;
            // Draw tick and label for val
            if (axisChar === 'x') { /* ... tick lines ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(val, -scaledTickLineLength/2, 0), new THREE.Vector3(val, scaledTickLineLength/2, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(val, 0, -scaledTickLineLength/2), new THREE.Vector3(val, 0, scaledTickLineLength/2)]), tickLineMaterial));
            } else if (axisChar === 'y') { /* ... tick lines ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, val, 0), new THREE.Vector3(scaledTickLineLength/2, val, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, val, -scaledTickLineLength/2), new THREE.Vector3(0, val, scaledTickLineLength/2)]), tickLineMaterial));
            } else { /* ... tick lines ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, 0, val), new THREE.Vector3(scaledTickLineLength/2, 0, val)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, -scaledTickLineLength/2, val), new THREE.Vector3(0, scaledTickLineLength/2, val)]), tickLineMaterial));
            }

            const labelPos = tickPos.clone();
            if (axisChar === 'x') { labelPos.y -= tickLabelOffsetFromLine; labelPos.z -= tickLabelOffsetFromLine; }
            else if (axisChar === 'y') { labelPos.x -= tickLabelOffsetFromLine; labelPos.z -= tickLabelOffsetFromLine; }
            else { labelPos.x -= tickLabelOffsetFromLine; labelPos.y -= tickLabelOffsetFromLine; }
            axisVisualsGroup.add(createTextLabel(String(val), null, labelPos, AXIS_TICK_LABEL_FONT_SIZE, AXIS_TICK_LABEL_COLOR, null, 1.0, false));

            if (val === 0 && visualMax === 0 && visualMin === 0) break;
            if (val <= visualMin && val !== 0) break; // Stop if we've reached or passed visualMin (and not at origin)
            if (tickStep === 0) break;
        }

        // Axis Name Label (X, Y, Z)
        const axisNameLabelPos = new THREE.Vector3();
        // Position name label at the positive extent of THIS axis's *actual drawn line* (visualMax)
        // If visualMax is 0 (e.g. data only negative), use defaultVisualExtent for label position
        const labelEndPos = (visualMax === 0 && visualMin < 0) ? Math.max(defaultVisualExtent, Math.abs(visualMin)) : visualMax; // Ensure label is at a visible positive end
        axisNameLabelPos[axisChar] = labelEndPos + tickLabelOffsetFromLine * 1.5;
        axisVisualsGroup.add(createTextLabel(axisChar.toUpperCase(), null, axisNameLabelPos, AXIS_TICK_LABEL_FONT_SIZE * 1.2, color, null, 1.2, true));
    }

    addTicks('x', rangeX.visualMin, rangeX.visualMax, new THREE.Color(0xff0000));
    addTicks('y', rangeY.visualMin, rangeY.visualMax, new THREE.Color(0x00ff00));
    addTicks('z', rangeZ.visualMin, rangeZ.visualMax, new THREE.Color(0x0000ff));

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
    const labelPosition = textPos3D.clone(); labelPosition[axis] += SLICE_PLANE_VISIBILITY_OFFSET * 2;
    const projLabel = createTextLabel(
        boxData.text1, boxData.text2, labelPosition,
        PROJECTION_LABEL_FONT_SIZE, PROJECTION_TEXT_COLOR, PROJECTION_TEXT_BG_COLOR, 0.8, true
    );
    projLabel.renderOrder = 1; projectionVisualsGroup.add(projLabel);
}

function updateSlicePlaneHelper(axis, coord) {
    let isHelperVisible = showSlicePlaneCheckbox.checked && axis !== 'none';
    if (show2DProjectionCheckbox.checked && axis !== 'none') {
        isHelperVisible = true;
    }
    slicePlaneHelper.visible = isHelperVisible;

    // --- Update Slice Axis Intersection Dot ---
    if (isHelperVisible && axis !== 'none') {
        sliceAxisIntersectionDot.visible = true;
        const dotPosition = new THREE.Vector3(0, 0, 0); // Start at origin
        dotPosition[axis] = coord; // Set the coordinate along the slice axis
        sliceAxisIntersectionDot.position.copy(dotPosition);

        // Scale dot size based on distance to camera to maintain somewhat constant screen size
        // This is a simple heuristic; more robust methods exist.
        const distance = camera.position.distanceTo(dotPosition);
        const scale = distance / 200; // Adjust divisor for desired screen size effect
        sliceAxisIntersectionDot.scale.set(scale, scale, scale);

    } else {
        sliceAxisIntersectionDot.visible = false;
    }
    // --- End Dot Update ---


    if (!isHelperVisible) return; // If helper plane isn't visible, nothing more to do

    const overallSize = new THREE.Vector3(); new THREE.Box3(sceneMinBounds, sceneMaxBounds).getSize(overallSize);
    const overallCenter = new THREE.Vector3(); new THREE.Box3(sceneMinBounds, sceneMaxBounds).getCenter(overallCenter);

    slicePlaneHelper.rotation.set(0,0,0);
    // The slice plane helper's position IS the coordinate along the axis,
    // but its geometry is centered, so we offset its geometry's center.
    // For simplicity, we set its position and let its geometry extend.

    if (slicePlaneHelper.geometry) slicePlaneHelper.geometry.dispose();

    let planeWidth = 1, planeHeight = 1;
    const planeCenterOffset = new THREE.Vector3(0,0,0); // Used to keep geometry centered for rotation

    if (axis === 'x') {
        planeWidth = Math.max(1, overallSize.y); planeHeight = Math.max(1, overallSize.z);
        slicePlaneHelper.geometry = new THREE.PlaneGeometry(planeWidth * 1.05, planeHeight * 1.05);
        slicePlaneHelper.rotation.y = Math.PI / 2;
        slicePlaneHelper.position.set(coord, overallCenter.y, overallCenter.z);
    } else if (axis === 'y') {
        planeWidth = Math.max(1, overallSize.x); planeHeight = Math.max(1, overallSize.z);
        slicePlaneHelper.geometry = new THREE.PlaneGeometry(planeWidth * 1.05, planeHeight * 1.05);
        slicePlaneHelper.rotation.x = Math.PI / 2;
        slicePlaneHelper.position.set(overallCenter.x, coord, overallCenter.z);
    } else if (axis === 'z') {
        planeWidth = Math.max(1, overallSize.x); planeHeight = Math.max(1, overallSize.y);
        slicePlaneHelper.geometry = new THREE.PlaneGeometry(planeWidth * 1.05, planeHeight * 1.05);
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
    if (axis === 'none' || (minVal === 0 && maxVal === 0 && step === 0.1) ) { // Simpler check for disabled state
        sliceCoordinateSlider.disabled = true; sliceCoordinateSlider.value = minVal;
    } else {
        sliceCoordinateSlider.disabled = false; const currentVal = parseFloat(sliceCoordinateSlider.value);
        if (isNaN(currentVal) || currentVal < minVal || currentVal > maxVal) { sliceCoordinateSlider.value = minVal; }
    }
    sliceCoordValueSpan.textContent = parseFloat(sliceCoordinateSlider.value).toFixed(2);
}

// --- Mouse Dragging Logic ---
function onDocumentMouseDown(event) {
    event.preventDefault(); mouse.x = (event.clientX/window.innerWidth)*2-1; mouse.y = -(event.clientY/window.innerHeight)*2+1;
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
        const sliderStepVal = parseFloat(sliceCoordinateSlider.step);
        if (Math.abs(parseFloat(sliceCoordinateSlider.value) - newCoord) > sliderStepVal / 2 || sliceCoordinateSlider.value == min || sliceCoordinateSlider.value == max) {
            sliceCoordinateSlider.value = newCoord.toFixed(Math.max(2, (String(sliderStepVal).split('.')[1] || '').length));
            updateSlice();
        }
    }
}
function onDocumentMouseUp() {
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