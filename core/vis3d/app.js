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
        // console.log(`Set page title to: "${document.title}"`);
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
const DISPLAY_SPACE_HALF_EXTENT = 10; // Visual cube will be from -10 to +10 on each axis

const LABEL_FONT_SIZE = 20; // Base font size for primary data box labels
const PROJECTION_LABEL_FONT_SIZE = 18;
const AXIS_TICK_LABEL_FONT_SIZE = 16; // Made slightly larger for visibility
const LINE_SPACING_FACTOR = 0.3;
const LABEL_TEXT_COLOR = 'white';
const LABEL_BG_COLOR = 'rgba(0, 0, 0, 0.6)';
const AXIS_TICK_LABEL_COLOR = 'lightgray';
const BOX_HELPER_COLOR = 0x00ff00;
const PROJECTION_OUTLINE_COLOR = 0xffa500;
const PROJECTION_TEXT_COLOR = 'black';
const PROJECTION_TEXT_BG_COLOR = 'rgba(255, 165, 0, 0.7)';
const SLICE_PLANE_VISIBILITY_OFFSET = DISPLAY_SPACE_HALF_EXTENT * 0.001; // Relative to display space
const AXIS_LABEL_OFFSET_BASE = DISPLAY_SPACE_HALF_EXTENT * 0.05; // Offset for X,Y,Z axis name labels
const AXIS_TICK_LINE_BASE_LENGTH = DISPLAY_SPACE_HALF_EXTENT * 0.02; // Tick line length relative to display space
const AXIS_TICK_LINE_COLOR = 0xaaaaaa;

const MIN_AXIS_LENGTH_FACTOR = 1.2; // How much axis line extends beyond data (in raw units, for calculation)
const TARGET_TICKS_PER_AXIS_SIDE = 4;
const SLICE_AXIS_DOT_COLOR = 0xffffff;


// --- Dragging State ---
const raycaster = new THREE.Raycaster(); const mouse = new THREE.Vector2();
let isDraggingSlicePlane = false; let dragPlane = new THREE.Plane();
let dragStartPointOnHelper = new THREE.Vector3(); let dragInitialCoordinate = 0;
let dragActiveAxis = null; const planeIntersectPoint = new THREE.Vector3();

// --- Helper: Map Raw Data Coordinate to Display Coordinate ---
function mapRawToDisplay(rawValue, rawMin, rawMax, displayMin, displayMax) {
    if (rawMin === rawMax) return (displayMin + displayMax) / 2;
    const normalized = (rawValue - rawMin) / (rawMax - rawMin);
    return normalized * (displayMax - displayMin) + displayMin;
}

// --- Helper: Map Display Coordinate back to Raw Data Coordinate ---
function mapDisplayToRaw(displayValue, rawMin, rawMax, displayMin, displayMax) {
    if (displayMin === displayMax) return (rawMin + rawMax) / 2;
    const normalized = (displayValue - displayMin) / (displayMax - displayMin);
    return normalized * (rawMax - rawMin) + rawMin;
}

// --- Main ---
init();
animate();

// --- Three.js Setup ---
function init() {
    scene = new THREE.Scene();
    scene.background = new THREE.Color(0x222222);

    defineDataBoxes();

    const displayCenter = new THREE.Vector3(0, 0, 0);
    const cameraDist = DISPLAY_SPACE_HALF_EXTENT * 2.8;

    camera = new THREE.PerspectiveCamera(60, window.innerWidth / window.innerHeight, 0.1, cameraDist * 5);
    camera.position.set(cameraDist * 0.7, cameraDist * 0.6, cameraDist);
    camera.lookAt(displayCenter);

    renderer = new THREE.WebGLRenderer({ antialias: true });
    renderer.setSize(window.innerWidth, window.innerHeight);
    document.getElementById('container').appendChild(renderer.domElement);

    controls = new OrbitControls(camera, renderer.domElement);
    controls.target.copy(displayCenter);
    // --- ADJUST CONTROL SPEEDS HERE ---
    controls.zoomSpeed = 15; // Custom zoom speed
    // controls.panSpeed = 1.5;   // Optional: Pan 1.5x faster
    // controls.rotateSpeed = 1.2; // Optional: Rotate 1.2x faster

    // To enable damping for smoother movement:
    // controls.enableDamping = true;
    // controls.dampingFactor = 0.05; // Optional: smoother controls
    controls.update();

    const ambientLight = new THREE.AmbientLight(0xffffff, 0.8); scene.add(ambientLight);
    const directionalLight = new THREE.DirectionalLight(0xffffff, 0.7);
    directionalLight.position.set(1, 1.5, 1).normalize(); scene.add(directionalLight);

    allVisualsGroup = new THREE.Group();
    dataBoxes.forEach(boxData => allVisualsGroup.add(createBoxVisual(boxData)));
    scene.add(allVisualsGroup);

    projectionVisualsGroup = new THREE.Group();
    scene.add(projectionVisualsGroup);

    const planeGeom = new THREE.PlaneGeometry(DISPLAY_SPACE_HALF_EXTENT * 2.2, DISPLAY_SPACE_HALF_EXTENT * 2.2);
    const planeMat = new THREE.MeshBasicMaterial({
        color: 0x00ffff, side: THREE.DoubleSide, transparent: true,
        opacity: 0.2, depthWrite: false
    });
    slicePlaneHelper = new THREE.Mesh(planeGeom, planeMat);
    slicePlaneHelper.name = "SlicePlaneHelper";
    scene.add(slicePlaneHelper);
    slicePlaneHelper.visible = false;

    // --- Create Slice Axis Intersection Dot ---
    const dotDisplaySize = DISPLAY_SPACE_HALF_EXTENT * 0.025; // Large dot
    const dotGeometry = new THREE.SphereGeometry(dotDisplaySize, 16, 8);
    const dotMaterial = new THREE.MeshBasicMaterial({ color: SLICE_AXIS_DOT_COLOR });
    sliceAxisIntersectionDot = new THREE.Mesh(dotGeometry, dotMaterial);
    sliceAxisIntersectionDot.visible = false;
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
                    dataBoxes = validatedBoxes; customDataProvided = true;
                    // console.log("Loaded data from URL.");
                } else { console.warn("URL data valid JSON but no valid boxes. Using default."); }
            } else { console.warn("URL data not valid array. Using default."); }
        }
    } catch (error) { console.error("Error parsing URL data:", error, ". Using default data."); }

    if (!customDataProvided) {
        // console.log("Using default data set.");
        dataBoxes = [
            { minBounds: new THREE.Vector3(0,0,0), maxBounds: new THREE.Vector3(1000000,1,1), text1: "X: 0 to 1M", text2: "Y/Z: 0 to 1" },
            { minBounds: new THREE.Vector3(-50,-500,-5), maxBounds: new THREE.Vector3(50,0,5), text1: "Y: -500 to 0" },
            { minBounds: new THREE.Vector3(2,6,200), maxBounds: new THREE.Vector3(8,10,800), text1: "Z: 200 to 800" },
        ];
    }

    sceneMinBounds = new THREE.Vector3(Infinity,Infinity,Infinity);
    sceneMaxBounds = new THREE.Vector3(-Infinity,-Infinity,-Infinity);
    if (dataBoxes.length > 0) {
        dataBoxes.forEach(box => {
            sceneMinBounds.min(box.minBounds);
            sceneMaxBounds.max(box.maxBounds);
        });
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
function createTextLabel( // textLine1, textLine2 can be numbers for axis labels
    textLine1, textLine2, rawPositionForLogic, // rawPosition used for logic if needed, but final pos is display
    fontSize, textColor, bgColor,
    textScaleFactor, bold
) {
    const displayPosition = new THREE.Vector3( // All labels are ultimately positioned in display space
        mapRawToDisplay(rawPositionForLogic.x, sceneMinBounds.x, sceneMaxBounds.x, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT),
        mapRawToDisplay(rawPositionForLogic.y, sceneMinBounds.y, sceneMaxBounds.y, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT),
        mapRawToDisplay(rawPositionForLogic.z, sceneMinBounds.z, sceneMaxBounds.z, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT)
    );

    const canvas = document.createElement('canvas'); const context = canvas.getContext('2d');
    const fontWeight = bold ? 'Bold ' : '';
    context.font = `${fontWeight}${fontSize}px Arial`;

    const lines = [];
    // Ensure text lines are strings for measureText and fillText
    if (textLine1 !== null && textLine1 !== undefined && String(textLine1).trim() !== "") lines.push(String(textLine1).trim());
    if (textLine2 !== null && textLine2 !== undefined && String(textLine2).trim() !== "") lines.push(String(textLine2).trim());
    if (lines.length === 0) lines.push(" ");

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

    if (bgColor) { /* ... background drawing ... */
        context.fillStyle = bgColor; context.beginPath(); const r = fontSize / 2.5;
        context.moveTo(r, 0); context.lineTo(canvas.width - r, 0); context.quadraticCurveTo(canvas.width, 0, canvas.width, r);
        context.lineTo(canvas.width, canvas.height - r); context.quadraticCurveTo(canvas.width, canvas.height, canvas.width - r, canvas.height);
        context.lineTo(r, canvas.height); context.quadraticCurveTo(0, canvas.height, 0, canvas.height - r);
        context.lineTo(0, r); context.quadraticCurveTo(0, 0, r, 0); context.closePath(); context.fill();
    }

    context.font = `${fontWeight}${fontSize}px Arial`;
    context.fillStyle = textColor;
    context.textAlign = 'center';
    context.textBaseline = 'middle';

    let startY = canvas.height / 2 - totalTextHeight / 2 + lineHeight / 2;

    lines.forEach((line, index) => {
        const yPos = startY + index * (lineHeight + lineSpacing);
        context.fillText(line, canvas.width / 2, yPos);
    });

    const texture = new THREE.CanvasTexture(canvas); texture.needsUpdate = true;
    const material = new THREE.SpriteMaterial({ map: texture, transparent: true, depthTest: true, depthWrite: true, sizeAttenuation: true });
    const sprite = new THREE.Sprite(material);

    // Sprite's world size should be relative to the DISPLAY_SPACE_HALF_EXTENT for consistency
    const desiredSpriteWorldHeight = textScaleFactor * DISPLAY_SPACE_HALF_EXTENT * 0.05 * (lines.length > 1 ? 1.3 : 1.0);
    // The 0.05 is a magic number for "default" sprite size relative to display space extent, adjust as needed
    // fontSize is an absolute px value, not directly usable for world scaling without camera params

    sprite.scale.set((desiredSpriteWorldHeight * canvas.width) / canvas.height, desiredSpriteWorldHeight, 1);
    // The `rawPositionForLogic` was already transformed to `displayPosition` for the sprite's final position.
    // For axis ticks, the position will be directly in display space.
    sprite.position.copy(displayPosition);
    return sprite;
}


function createBoxVisual(boxData) { /* ... (uses createTextLabel with raw center) ... */
    const group = new THREE.Group(); group.userData.boxData = boxData;
    const displayMin = new THREE.Vector3(
        mapRawToDisplay(boxData.minBounds.x, sceneMinBounds.x, sceneMaxBounds.x, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT),
        mapRawToDisplay(boxData.minBounds.y, sceneMinBounds.y, sceneMaxBounds.y, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT),
        mapRawToDisplay(boxData.minBounds.z, sceneMinBounds.z, sceneMaxBounds.z, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT)
    );
    const displayMax = new THREE.Vector3(
        mapRawToDisplay(boxData.maxBounds.x, sceneMinBounds.x, sceneMaxBounds.x, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT),
        mapRawToDisplay(boxData.maxBounds.y, sceneMinBounds.y, sceneMaxBounds.y, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT),
        mapRawToDisplay(boxData.maxBounds.z, sceneMinBounds.z, sceneMaxBounds.z, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT)
    );
    const displayBox3 = new THREE.Box3(displayMin, displayMax);
    const boxHelper = new THREE.Box3Helper(displayBox3, BOX_HELPER_COLOR); group.add(boxHelper);
    const rawCenter = new THREE.Vector3(); new THREE.Box3(boxData.minBounds, boxData.maxBounds).getCenter(rawCenter);
    const label = createTextLabel(
        boxData.text1, boxData.text2, rawCenter, // Pass raw center
        LABEL_FONT_SIZE, LABEL_TEXT_COLOR, LABEL_BG_COLOR, 1.0, true
    );
    group.add(label); return group;
}

// Helper function to calculate "nice" step for axis ticks
function getNiceTickStep(dataRangeForStepCalc) {
    if (dataRangeForStepCalc <= 0) return 1;
    let roughStep = dataRangeForStepCalc / TARGET_TICKS_PER_AXIS_SIDE;
    if (roughStep === 0) return 1;
    const exponent = Math.floor(Math.log10(roughStep));
    const significand = roughStep / Math.pow(10, exponent);

    let niceSignificand;
    if (significand <= 1.0) niceSignificand = 1.0;
    else if (significand <= 2.0) niceSignificand = 2.0;
    else if (significand <= 5.0) niceSignificand = 5.0;
    else niceSignificand = 10.0;

    let niceStep = niceSignificand * Math.pow(10, exponent);

    return Math.max(1, niceStep);
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

    const axisLineDisplayLength = DISPLAY_SPACE_HALF_EXTENT; // Visual lines span the display cube
    const xAxisMat = new THREE.LineBasicMaterial({ color: 0xff0000 });
    axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-axisLineDisplayLength,0,0), new THREE.Vector3(axisLineDisplayLength,0,0)]), xAxisMat));
    const yAxisMat = new THREE.LineBasicMaterial({ color: 0x008f00 });
    axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0,-axisLineDisplayLength,0), new THREE.Vector3(0,axisLineDisplayLength,0)]), yAxisMat));
    const zAxisMat = new THREE.LineBasicMaterial({ color: 0x0000ff });
    axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0,0,-axisLineDisplayLength), new THREE.Vector3(0,0,axisLineDisplayLength)]), zAxisMat));

    const tickLineMaterial = new THREE.LineBasicMaterial({ color: AXIS_TICK_LINE_COLOR });
    const scaledTickLineLength = AXIS_TICK_LINE_BASE_LENGTH; // Already relative to display space
    const tickLabelOffsetFromLine = AXIS_LABEL_OFFSET_BASE * 0.5; // Adjusted for ticks, relative to display space

    function addTicks(axisChar, rawDataMin, rawDataMax, color) {
        const dataRangeRaw = rawDataMax - rawDataMin;
        if (dataRangeRaw === 0 && !(rawDataMin === 0 && rawDataMax ===0)) { // Single point data not at origin
             const displayPosSingle = mapRawToDisplay(rawDataMin, rawDataMin, rawDataMax, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT);
             const tickPos = new THREE.Vector3(); tickPos[axisChar] = displayPosSingle;
             const labelPos = tickPos.clone(); // This labelPos is in DISPLAY SPACE
             if (axisChar === 'x') { labelPos.y -= tickLabelOffsetFromLine; labelPos.z -= tickLabelOffsetFromLine;}
             else if (axisChar === 'y') { labelPos.x -= tickLabelOffsetFromLine; labelPos.z -= tickLabelOffsetFromLine;}
             else { labelPos.x -= tickLabelOffsetFromLine; labelPos.y -= tickLabelOffsetFromLine;}
             // For createTextLabel, the position is used to set sprite.position directly, so it must be display space
             // The first two args are text lines, third is position for the sprite.
             // For axis ticks, rawPositionForLogic isn't strictly needed if pos is already display.
             // But createTextLabel expects a rawPos to transform. So we give it a dummy raw (0,0,0)
             // and then override sprite.position after creation.
             const tickLabelSprite = createTextLabel(String(rawDataMin.toFixed(1)), null, new THREE.Vector3(), AXIS_TICK_LABEL_FONT_SIZE, AXIS_TICK_LABEL_COLOR, null, 0.6, false); // Smaller scaleFactor
             tickLabelSprite.position.copy(labelPos); // Explicitly set display position
             axisVisualsGroup.add(tickLabelSprite);
             return;
        }
        if (dataRangeRaw === 0 && rawDataMin === 0 && rawDataMax === 0 && axisChar !=='x') return;

        const tickStepRaw = getNiceTickStep(Math.max(Math.abs(rawDataMin), Math.abs(rawDataMax)));

        // Positive Ticks (and 0)
        for (let currentRawVal = 0; ; currentRawVal += tickStepRaw) {
            if (currentRawVal > rawDataMax && currentRawVal !== 0 && !(rawDataMax === 0 && rawDataMin < 0)) break;
            if (currentRawVal < rawDataMin && currentRawVal !== 0) {
                if (currentRawVal + tickStepRaw > rawDataMin || rawDataMin === 0) {} else { if (tickStepRaw === 0) break; continue; }
            }
            const displayVal = mapRawToDisplay(currentRawVal, rawDataMin, rawDataMax, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT);
            if (displayVal > DISPLAY_SPACE_HALF_EXTENT + 0.01 && currentRawVal !==0) break;

            const tickPosDisplay = new THREE.Vector3(); tickPosDisplay[axisChar] = displayVal;
            if (axisChar === 'x') { /* ... tick lines at tickPosDisplay ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(displayVal, -scaledTickLineLength/2, 0), new THREE.Vector3(displayVal, scaledTickLineLength/2, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(displayVal, 0, -scaledTickLineLength/2), new THREE.Vector3(displayVal, 0, scaledTickLineLength/2)]), tickLineMaterial));
            } else if (axisChar === 'y') { /* ... tick lines at tickPosDisplay ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, displayVal, 0), new THREE.Vector3(scaledTickLineLength/2, displayVal, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, displayVal, -scaledTickLineLength/2), new THREE.Vector3(0, displayVal, scaledTickLineLength/2)]), tickLineMaterial));
            } else { /* ... tick lines at tickPosDisplay ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, 0, displayVal), new THREE.Vector3(scaledTickLineLength/2, 0, displayVal)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, -scaledTickLineLength/2, displayVal), new THREE.Vector3(0, scaledTickLineLength/2, displayVal)]), tickLineMaterial));
            }
            const labelPosDisplay = tickPosDisplay.clone();
            if (axisChar === 'x') { labelPosDisplay.y -= tickLabelOffsetFromLine; labelPosDisplay.z -= tickLabelOffsetFromLine;}
            else if (axisChar === 'y') { labelPosDisplay.x -= tickLabelOffsetFromLine; labelPosDisplay.z -= tickLabelOffsetFromLine;}
            else { labelPosDisplay.x -= tickLabelOffsetFromLine; labelPosDisplay.y -= tickLabelOffsetFromLine;}
            if (currentRawVal === 0 && axisChar !== 'x') {}
            else {
                const tickLabelSprite = createTextLabel(String(currentRawVal), null, new THREE.Vector3(), AXIS_TICK_LABEL_FONT_SIZE, AXIS_TICK_LABEL_COLOR, null, 0.7, false); // textScaleFactor 0.7
                tickLabelSprite.position.copy(labelPosDisplay); // Set final position in display space
                axisVisualsGroup.add(tickLabelSprite);
            }
            if (currentRawVal === 0 && rawDataMax === 0 && rawDataMin === 0) break;
            if (currentRawVal >= rawDataMax && currentRawVal !==0) break;
            if (tickStepRaw === 0) break;
        }
        // Negative Ticks
        for (let currentRawVal = -tickStepRaw; ; currentRawVal -= tickStepRaw) {
            if (currentRawVal < rawDataMin && currentRawVal !== 0 && !(rawDataMin === 0 && rawDataMax > 0)) break;
            if (currentRawVal > rawDataMax && currentRawVal !== 0) {
                 if (currentRawVal - tickStepRaw < rawDataMax || rawDataMax === 0) {} else { if (tickStepRaw === 0) break; continue; }
            }
            const displayVal = mapRawToDisplay(currentRawVal, rawDataMin, rawDataMax, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT);
            if (displayVal < -DISPLAY_SPACE_HALF_EXTENT - 0.01 && currentRawVal !==0) break;

            const tickPosDisplay = new THREE.Vector3(); tickPosDisplay[axisChar] = displayVal;
            // Tick Lines ...
            if (axisChar === 'x') { /* ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(displayVal, -scaledTickLineLength/2, 0), new THREE.Vector3(displayVal, scaledTickLineLength/2, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(displayVal, 0, -scaledTickLineLength/2), new THREE.Vector3(displayVal, 0, scaledTickLineLength/2)]), tickLineMaterial));
            } else if (axisChar === 'y') { /* ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, displayVal, 0), new THREE.Vector3(scaledTickLineLength/2, displayVal, 0)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, displayVal, -scaledTickLineLength/2), new THREE.Vector3(0, displayVal, scaledTickLineLength/2)]), tickLineMaterial));
            } else { /* ... */
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(-scaledTickLineLength/2, 0, displayVal), new THREE.Vector3(scaledTickLineLength/2, 0, displayVal)]), tickLineMaterial));
                axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, -scaledTickLineLength/2, displayVal), new THREE.Vector3(0, scaledTickLineLength/2, displayVal)]), tickLineMaterial));
            }
            const labelPosDisplay = tickPosDisplay.clone();
            if (axisChar === 'x') { labelPosDisplay.y -= tickLabelOffsetFromLine; labelPosDisplay.z -= tickLabelOffsetFromLine;}
            else if (axisChar === 'y') { labelPosDisplay.x -= tickLabelOffsetFromLine; labelPosDisplay.z -= tickLabelOffsetFromLine;}
            else { labelPosDisplay.x -= tickLabelOffsetFromLine; labelPosDisplay.y -= tickLabelOffsetFromLine;}
            const tickLabelSprite = createTextLabel(String(currentRawVal), null, new THREE.Vector3(), AXIS_TICK_LABEL_FONT_SIZE, AXIS_TICK_LABEL_COLOR, null, 0.7, false); // textScaleFactor 0.7
            tickLabelSprite.position.copy(labelPosDisplay); // Set final position
            axisVisualsGroup.add(tickLabelSprite);
            if (currentRawVal === 0 && rawDataMax === 0 && rawDataMin === 0) break;
            if (currentRawVal <= rawDataMin && currentRawVal !==0) break;
            if (tickStepRaw === 0) break;
        }
        const axisNameLabelPos = new THREE.Vector3();
        axisNameLabelPos[axisChar] = DISPLAY_SPACE_HALF_EXTENT + tickLabelOffsetFromLine * 1.5;
        // For axis name label, pass its display position directly as the "rawPositionForLogic"
        // because it doesn't correspond to a specific raw data value for mapping.
        // Or, more simply, adapt createTextLabel to optionally take a pre-transformed display position.
        // For now, we'll use a small hack: pass the display position, and a dummy raw range that maps 1:1.
        // A better way: modify createTextLabel to accept displayPosition directly.
        // Let's try a simpler approach for axis names: position them after creation.
        const axisNameSprite = createTextLabel(axisChar.toUpperCase(), null, new THREE.Vector3() /*dummy raw*/, AXIS_TICK_LABEL_FONT_SIZE*1.2, color, null, 1.0, true); // Larger scaleFactor for axis name
        axisNameSprite.position.copy(axisNameLabelPos); // Explicitly set its display position
        axisVisualsGroup.add(axisNameSprite);
    }
    addTicks('x', sceneMinBounds.x, sceneMaxBounds.x, new THREE.Color(0xff0000));
    addTicks('y', sceneMinBounds.y, sceneMaxBounds.y, new THREE.Color(0x00ff00));
    addTicks('z', sceneMinBounds.z, sceneMaxBounds.z, new THREE.Color(0x0000ff));
    scene.add(axisVisualsGroup);
}


// --- Slice Logic - ensure all rawCoord -> displayCoord transforms are in place ...
// ... (Double-check create2DProjection for label positioning using transformed coordinates)
function updateSlice() {
    const axis = sliceAxisSelect.value;
    const rawSliceCoord = parseFloat(sliceCoordinateSlider.value);
    sliceCoordValueSpan.textContent = rawSliceCoord.toFixed(2);
    const do2DProjection = show2DProjectionCheckbox.checked && axis !== 'none';
    clearProjectionVisuals();
    allVisualsGroup.children.forEach(visualGroup => {
        const boxData = visualGroup.userData.boxData;
        if (!boxData) return;
        let intersectsRaw = false;
        if (axis !== 'none') {
            if (axis === 'x') intersectsRaw = (boxData.minBounds.x <= rawSliceCoord && boxData.maxBounds.x >= rawSliceCoord);
            else if (axis === 'y') intersectsRaw = (boxData.minBounds.y <= rawSliceCoord && boxData.maxBounds.y >= rawSliceCoord);
            else if (axis === 'z') intersectsRaw = (boxData.minBounds.z <= rawSliceCoord && boxData.maxBounds.z >= rawSliceCoord);
        }
        if (axis === 'none') visualGroup.visible = true;
        else {
            if (do2DProjection) {
                visualGroup.visible = false;
                if (intersectsRaw) create2DProjection(boxData, axis, rawSliceCoord);
            } else visualGroup.visible = intersectsRaw;
        }
    });
    updateSlicePlaneHelper(axis, rawSliceCoord);
    projectionVisualsGroup.visible = do2DProjection;
}

function clearProjectionVisuals() {
    while (projectionVisualsGroup.children.length > 0) {
        const child = projectionVisualsGroup.children[0]; projectionVisualsGroup.remove(child);
        if (child.geometry) child.geometry.dispose();
        if (child.material) { if (child.material.map) child.material.map.dispose(); child.material.dispose(); }
    }
}

function create2DProjection(boxData, axis, rawSliceCoord) {
    // Calculate the display coordinate of the slice plane
    let displaySliceCoordPlane = 0;
    if (axis === 'x') displaySliceCoordPlane = mapRawToDisplay(rawSliceCoord, sceneMinBounds.x, sceneMaxBounds.x, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT);
    else if (axis === 'y') displaySliceCoordPlane = mapRawToDisplay(rawSliceCoord, sceneMinBounds.y, sceneMaxBounds.y, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT);
    else if (axis === 'z') displaySliceCoordPlane = mapRawToDisplay(rawSliceCoord, sceneMinBounds.z, sceneMaxBounds.z, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT);

    // Calculate projected rectangle corners in RAW data units on the slice plane
    let projMinRaw = new THREE.Vector2();
    let projMaxRaw = new THREE.Vector2();
    if (axis === 'x') { projMinRaw.set(boxData.minBounds.y, boxData.minBounds.z); projMaxRaw.set(boxData.maxBounds.y, boxData.maxBounds.z); }
    else if (axis === 'y') { projMinRaw.set(boxData.minBounds.x, boxData.minBounds.z); projMaxRaw.set(boxData.maxBounds.x, boxData.maxBounds.z); }
    else { projMinRaw.set(boxData.minBounds.x, boxData.minBounds.y); projMaxRaw.set(boxData.maxBounds.x, boxData.maxBounds.y); }

    const widthRaw = projMaxRaw.x - projMinRaw.x;
    const heightRaw = projMaxRaw.y - projMinRaw.y;
    if (widthRaw <= 0 || heightRaw <= 0) return;

    // Define the 4 corners of the rectangle in RAW 3D space, lying on the slice plane
    const cornersRaw = [
        new THREE.Vector3(), new THREE.Vector3(), new THREE.Vector3(), new THREE.Vector3()
    ];
    if (axis === 'x') {
        cornersRaw[0].set(rawSliceCoord, projMinRaw.x, projMinRaw.y);
        cornersRaw[1].set(rawSliceCoord, projMaxRaw.x, projMinRaw.y);
        cornersRaw[2].set(rawSliceCoord, projMaxRaw.x, projMaxRaw.y);
        cornersRaw[3].set(rawSliceCoord, projMinRaw.x, projMaxRaw.y);
    } else if (axis === 'y') {
        cornersRaw[0].set(projMinRaw.x, rawSliceCoord, projMinRaw.y);
        cornersRaw[1].set(projMaxRaw.x, rawSliceCoord, projMinRaw.y);
        cornersRaw[2].set(projMaxRaw.x, rawSliceCoord, projMaxRaw.y);
        cornersRaw[3].set(projMinRaw.x, rawSliceCoord, projMaxRaw.y);
    } else { // Z-axis
        cornersRaw[0].set(projMinRaw.x, projMinRaw.y, rawSliceCoord);
        cornersRaw[1].set(projMaxRaw.x, projMinRaw.y, rawSliceCoord);
        cornersRaw[2].set(projMaxRaw.x, projMaxRaw.y, rawSliceCoord);
        cornersRaw[3].set(projMinRaw.x, projMaxRaw.y, rawSliceCoord);
    }

    // Transform these RAW corners to DISPLAY space
    const displayOutlinePoints = cornersRaw.map(cornerRaw => new THREE.Vector3(
        mapRawToDisplay(cornerRaw.x, sceneMinBounds.x, sceneMaxBounds.x, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT),
        mapRawToDisplay(cornerRaw.y, sceneMinBounds.y, sceneMaxBounds.y, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT),
        mapRawToDisplay(cornerRaw.z, sceneMinBounds.z, sceneMaxBounds.z, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT)
    ));

    // The points are now coplanar in display space at the displaySliceCoordPlane value for the given axis.
    const outlineGeom = new THREE.BufferGeometry().setFromPoints(displayOutlinePoints);
    const outlineMat = new THREE.LineBasicMaterial({ color: PROJECTION_OUTLINE_COLOR, linewidth: 2, transparent: true, opacity: 0.8, depthTest: false }); // depthTest: false for outline
    const outline = new THREE.LineLoop(outlineGeom, outlineMat);
    // No need to further position outline.position[axis] as the points are already correct.
    // The `depthTest: false` will make it render on top of the slicePlaneHelper if at same depth.
    // Alternatively, slightly offset the entire outline along the plane's normal.
    // For simplicity with rotations, `depthTest: false` is easier here.
    // Or, ensure SLICE_PLANE_VISIBILITY_OFFSET is small and applied to the *world* normal of the plane
    // For now, let's rely on render order and depthTest for the outline.
    outline.renderOrder = 1; // Render outline after plane helper
    projectionVisualsGroup.add(outline);


    // --- Text Label ---
    // Calculate RAW center of the projected rectangle for the label's logical position
    let textCenterRaw = new THREE.Vector3();
    if (axis === 'x') textCenterRaw.set(rawSliceCoord, (projMinRaw.x + projMaxRaw.x) / 2, (projMinRaw.y + projMaxRaw.y) / 2);
    else if (axis === 'y') textCenterRaw.set((projMinRaw.x + projMaxRaw.x) / 2, rawSliceCoord, (projMinRaw.y + projMaxRaw.y) / 2);
    else textCenterRaw.set((projMinRaw.x + projMaxRaw.x) / 2, (projMinRaw.y + projMaxRaw.y) / 2, rawSliceCoord);

    // Transform this raw center to display space for the sprite's final position
    const textCenterDisplay = new THREE.Vector3(
        mapRawToDisplay(textCenterRaw.x, sceneMinBounds.x, sceneMaxBounds.x, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT),
        mapRawToDisplay(textCenterRaw.y, sceneMinBounds.y, sceneMaxBounds.y, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT),
        mapRawToDisplay(textCenterRaw.z, sceneMinBounds.z, sceneMaxBounds.z, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT)
    );

    // Create the label. createTextLabel positions it based on its internal transformation of rawPositionForLogic.
    // We need its final position to be textCenterDisplay, slightly offset.
    const projLabel = createTextLabel(
        boxData.text1, boxData.text2,
        textCenterRaw, // Pass raw center for createTextLabel's internal mapping logic
        PROJECTION_LABEL_FONT_SIZE, PROJECTION_TEXT_COLOR, PROJECTION_TEXT_BG_COLOR,
        0.8, true
    );

    // Now, ensure the sprite's final position is the calculated display center,
    // potentially offset slightly from the plane for visibility.
    // The slicePlaneHelper is at displaySliceCoordPlane.
    // The textCenterDisplay also has its 'axis' component at displaySliceCoordPlane.
    // We need a *small* offset along the world normal of the plane helper.

    const planeWorldNormal = new THREE.Vector3();
    if (axis === 'x') planeWorldNormal.set(1,0,0);
    else if (axis === 'y') planeWorldNormal.set(0,1,0);
    else planeWorldNormal.set(0,0,1);
    // If slicePlaneHelper is rotated, its world normal would be different.
    // For simplicity, assuming slicePlaneHelper rotation aligns its local Z with the slice axis normal.
    // A more robust way if slicePlaneHelper had complex rotations:
    // slicePlaneHelper.getWorldDirection(planeWorldNormal); planeWorldNormal.negate(); // Normal points away from front face

    const finalLabelPosDisplay = textCenterDisplay.clone().addScaledVector(planeWorldNormal, SLICE_PLANE_VISIBILITY_OFFSET * 2);
    projLabel.position.copy(finalLabelPosDisplay);

    projLabel.renderOrder = 2; // Render label on top of outline and plane helper
    projectionVisualsGroup.add(projLabel);
}

function updateSlicePlaneHelper(axis, rawSliceCoord) { 
    let isHelperVisible = showSlicePlaneCheckbox.checked && axis !== 'none';
    if (show2DProjectionCheckbox.checked && axis !== 'none') isHelperVisible = true;
    slicePlaneHelper.visible = isHelperVisible;
    let displaySliceCoord = 0;
    if (axis === 'x') displaySliceCoord = mapRawToDisplay(rawSliceCoord, sceneMinBounds.x, sceneMaxBounds.x, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT);
    else if (axis === 'y') displaySliceCoord = mapRawToDisplay(rawSliceCoord, sceneMinBounds.y, sceneMaxBounds.y, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT);
    else if (axis === 'z') displaySliceCoord = mapRawToDisplay(rawSliceCoord, sceneMinBounds.z, sceneMaxBounds.z, -DISPLAY_SPACE_HALF_EXTENT, DISPLAY_SPACE_HALF_EXTENT);
    else if (axis === 'none') { sliceAxisIntersectionDot.visible = false; return; }


    if (isHelperVisible) {
        sliceAxisIntersectionDot.visible = true;
        const dotDisplayPosition = new THREE.Vector3(0,0,0);
        dotDisplayPosition[axis] = displaySliceCoord;
        sliceAxisIntersectionDot.position.copy(dotDisplayPosition);
    } else {
        sliceAxisIntersectionDot.visible = false;
    }
    if (!isHelperVisible) return;
    slicePlaneHelper.rotation.set(0,0,0);
    const helperPosition = new THREE.Vector3(0,0,0);
    helperPosition[axis] = displaySliceCoord;
    slicePlaneHelper.position.copy(helperPosition);
    if (axis === 'x') slicePlaneHelper.rotation.y = Math.PI / 2;
    else if (axis === 'y') slicePlaneHelper.rotation.x = Math.PI / 2;
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
    if (axis === 'none' || (dataBoxes.length === 0)) { // Simplified disable condition
        sliceCoordinateSlider.disabled = true; sliceCoordinateSlider.value = minVal;
    } else {
        sliceCoordinateSlider.disabled = false; const currentVal = parseFloat(sliceCoordinateSlider.value);
        if (isNaN(currentVal) || currentVal < minVal || currentVal > maxVal) { sliceCoordinateSlider.value = minVal; }
    }
    if (document.getElementById('sliceCoordValue')) { // Check if span exists
      sliceCoordValueSpan.textContent = parseFloat(sliceCoordinateSlider.value).toFixed(2);
    }
}

// --- Mouse Dragging Logic ---
function onDocumentMouseDown(event) {
    event.preventDefault(); mouse.x = (event.clientX/window.innerWidth)*2-1; mouse.y = -(event.clientY/window.innerHeight)*2+1;
    raycaster.setFromCamera(mouse, camera); const currentSliceAxis = sliceAxisSelect.value;
    if (currentSliceAxis === 'none' || !slicePlaneHelper.visible) return;
    const intersects = raycaster.intersectObject(slicePlaneHelper, false);
    if (intersects.length > 0) {
        isDraggingSlicePlane = true; controls.enabled = false; renderer.domElement.style.cursor = 'grabbing';
        dragActiveAxis = currentSliceAxis;
        dragInitialCoordinate = parseFloat(sliceCoordinateSlider.value);
        dragStartPointOnHelper.copy(intersects[0].point);
        camera.getWorldDirection(dragPlane.normal); dragPlane.setFromNormalAndCoplanarPoint(dragPlane.normal, dragStartPointOnHelper);
    }
}
function onDocumentMouseMove(event) { /* ... (ensures raw coordinate change) ... */
    event.preventDefault(); mouse.x = (event.clientX/window.innerWidth)*2-1; mouse.y = -(event.clientY/window.innerHeight)*2+1;
    if (!isDraggingSlicePlane && sliceAxisSelect.value !== 'none' && slicePlaneHelper.visible) {
        raycaster.setFromCamera(mouse, camera); const intersects = raycaster.intersectObject(slicePlaneHelper, false);
        renderer.domElement.style.cursor = intersects.length > 0 ? 'grab' : 'default';
    }
    if (!isDraggingSlicePlane) return;
    raycaster.setFromCamera(mouse, camera);
    if (raycaster.ray.intersectPlane(dragPlane, planeIntersectPoint)) {
        const dragVectorDisplay = planeIntersectPoint.clone().sub(dragStartPointOnHelper);
        let rawMin, rawMax;
        if (dragActiveAxis === 'x') { rawMin = sceneMinBounds.x; rawMax = sceneMaxBounds.x; }
        else if (dragActiveAxis === 'y') { rawMin = sceneMinBounds.y; rawMax = sceneMaxBounds.y; }
        else { rawMin = sceneMinBounds.z; rawMax = sceneMaxBounds.z; }
        let coordinateChangeRaw = 0;
        const rawDataRange = rawMax - rawMin;
        const displayDataRange = DISPLAY_SPACE_HALF_EXTENT * 2;
        if (displayDataRange !== 0 && rawDataRange !== 0) {
            if (dragActiveAxis === 'x') coordinateChangeRaw = dragVectorDisplay.x * (rawDataRange / displayDataRange);
            else if (dragActiveAxis === 'y') coordinateChangeRaw = dragVectorDisplay.y * (rawDataRange / displayDataRange);
            else if (dragActiveAxis === 'z') coordinateChangeRaw = dragVectorDisplay.z * (rawDataRange / displayDataRange);
        }
        let newRawCoordinate = dragInitialCoordinate + coordinateChangeRaw;
        newRawCoordinate = Math.max(rawMin, Math.min(rawMax, newRawCoordinate));
        const sliderStepVal = parseFloat(sliceCoordinateSlider.step);
        const currentValue = parseFloat(sliceCoordinateSlider.value);
        if (Math.abs(currentValue - newRawCoordinate) > sliderStepVal / 2 || currentValue == rawMin || currentValue == rawMax ) {
            sliceCoordinateSlider.value = newRawCoordinate.toFixed(Math.max(2, (String(sliderStepVal).split('.')[1] || '').length));
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
function animate() { requestAnimationFrame(animate); if(controls.enableDamping) controls.update(); renderer.render(scene, camera); }
function onWindowResize() {
    camera.aspect = window.innerWidth / window.innerHeight; camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
}

// Initial call
updateSliderRange();
updateSlice();