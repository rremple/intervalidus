import * as THREE from 'three';
import { OrbitControls } from 'three/addons/controls/OrbitControls.js';

// 3D visualizer, 100% vibe-coded using Gemini 2.5 Pro Preview 05-06
// Renders the non-metric representation of data, allowing it to be rotated, sliced, and understood.
// Launched by core/src/test/scala/intervalidus/Visualize3D.scala

// --- Set Custom Title from URL Parameter ---
try {
    const urlParams = new URLSearchParams(window.location.search);
    const customTitle = urlParams.get('title');
    if (customTitle && customTitle.trim() !== '') document.title = decodeURIComponent(customTitle.trim());
} catch (error) { console.error("Error processing title URL parameter:", error); }

let scene, camera, renderer, controls;
let allVisualsGroup;
let slicePlaneHelper;
let dataBoxes = [];
let axisBoundaryMaps = { x: [], y: [], z: [] };
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
const VISUAL_GRID_SPACING = 3;
const LABEL_FONT_SIZE = 20;
const AXIS_TICK_LABEL_FONT_SIZE = 16;
const LABEL_TEXT_COLOR = 'white';
const LABEL_BG_COLOR = 'rgba(0, 0, 0, 0.6)';
const AXIS_TICK_LABEL_COLOR = 'lightgray';
const BOX_HELPER_COLOR = 0x00ff00;
const PROJECTION_OUTLINE_COLOR = 0xffa500;
const SLICE_AXIS_DOT_COLOR = 0xffffff;


// --- Dragging State ---
const raycaster = new THREE.Raycaster();
const mouse = new THREE.Vector2();
let isDraggingSlicePlane = false;
let dragPlane = new THREE.Plane();
let dragStartPointOnHelper = new THREE.Vector3(); // Initial intersection in world space
let dragInitialIndex = 0; // The slice index when the drag started
let dragActiveAxis = null; // 'x', 'y', or 'z'
const planeIntersectPoint = new THREE.Vector3(); // Reusable vector

// --- Helper: Get Display Coordinate ---
function getDisplayCoord(boundaryValue, axis) {
    let searchValue = boundaryValue;
    if (boundaryValue === Number.POSITIVE_INFINITY) searchValue = "+∞";
    if (boundaryValue === Number.NEGATIVE_INFINITY) searchValue = "-∞";
    const index = axisBoundaryMaps[axis].indexOf(searchValue);
    if (index === -1) {
        console.warn(`Boundary value "${boundaryValue}" not found in axis "${axis}" map.`);
        return 0;
    }
    return index * VISUAL_GRID_SPACING;
}

// --- Main ---
init();
animate();

// --- Three.js Setup ---
function init() {
    scene = new THREE.Scene();
    scene.background = new THREE.Color(0x222222);

    defineDataBoxes();

    const vizSizeX = (axisBoundaryMaps.x.length - 1) * VISUAL_GRID_SPACING;
    const vizSizeY = (axisBoundaryMaps.y.length - 1) * VISUAL_GRID_SPACING;
    const vizSizeZ = (axisBoundaryMaps.z.length - 1) * VISUAL_GRID_SPACING;
    const vizCenter = new THREE.Vector3(vizSizeX / 2, vizSizeY / 2, vizSizeZ / 2);
    const maxVizDim = Math.max(vizSizeX, vizSizeY, vizSizeZ, 1);

    camera = new THREE.PerspectiveCamera(60, window.innerWidth / window.innerHeight, 0.1, maxVizDim * 5);
    camera.position.set(vizCenter.x + maxVizDim, vizCenter.y + maxVizDim * 0.8, vizCenter.z + maxVizDim);
    camera.lookAt(vizCenter);

    renderer = new THREE.WebGLRenderer({ antialias: true });
    renderer.setSize(window.innerWidth, window.innerHeight);
    document.getElementById('container').appendChild(renderer.domElement);

    controls = new OrbitControls(camera, renderer.domElement);
    controls.target.copy(vizCenter);
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

    slicePlaneHelper = new THREE.Mesh(new THREE.PlaneGeometry(1, 1), new THREE.MeshBasicMaterial({ color: 0x00ffff, side: THREE.DoubleSide, transparent: true, opacity: 0.2, depthWrite: false }));
    slicePlaneHelper.name = "SlicePlaneHelper"; scene.add(slicePlaneHelper); slicePlaneHelper.visible = false;
    slicePlaneHelper.renderOrder = -1;

    // --- Create Slice Axis Intersection Dot ---
    const dotDisplaySize = VISUAL_GRID_SPACING * 0.05;
    sliceAxisIntersectionDot = new THREE.Mesh(new THREE.SphereGeometry(dotDisplaySize, 16, 8), new THREE.MeshBasicMaterial({ color: SLICE_AXIS_DOT_COLOR }));
    sliceAxisIntersectionDot.visible = false; scene.add(sliceAxisIntersectionDot);
    // --- End Dot Creation ---

    createAxisVisuals();

    // Event Listeners
    sliceAxisSelect.addEventListener('change', () => { updateSliderRange(); updateSlice(); });
    sliceCoordinateSlider.addEventListener('input', updateSlice);
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
    console.log("in defineDataBoxes")
    let axesData = null; let boxDefData = null; let customDataProvided = false;
    try {
        const urlParams = new URLSearchParams(window.location.search);
        const axesParam = urlParams.get('axes');
        const dataParam = urlParams.get('data');
        if (axesParam) axesData = JSON.parse(decodeURIComponent(axesParam));
        if (dataParam) boxDefData = JSON.parse(decodeURIComponent(dataParam));
        console.log("axesData=", axesData)
        console.log("boxDefData=", boxDefData)
        if (axesData && boxDefData && Array.isArray(boxDefData)) {
            axisBoundaryMaps.x = axesData.x || []; axisBoundaryMaps.y = axesData.y || []; axisBoundaryMaps.z = axesData.z || [];
            dataBoxes = boxDefData.map(item => ({ minBounds: item.min, maxBounds: item.max, text1: item.text1 || item.text || "", text2: item.text2 || "" }));
            customDataProvided = true;
        }
    } catch (error) { console.error("Error parsing URL data:", error, ". Using default data."); }

    if (!customDataProvided) {
        // console.log("Using default data set.");
        axisBoundaryMaps = {
            x: ["-∞", "red", "blue", "+∞"],
            y: ["-∞", "red", "blue", "violet", "+∞"],
            z: ["-∞", "green", "blue", "+∞"]
        };
        dataBoxes = [
            { minBounds: ["-∞", "red", "blue"], maxBounds: ["blue", "blue", "+∞"], text1: "Box 1" },
            { minBounds: ["red", "red", "green"], maxBounds: ["blue", "violet", "+∞"], text1: "Box 2" },
            { minBounds: ["blue", "violet", "-∞"], maxBounds: ["+∞", "+∞", "green"], text1: "Box 3", text2: "Spans axes" }
        ];
    }
}

// --- Visualization Creation ---
function createTextLabel(textLine1, textLine2, position, fontSize, textColor, bgColor, bold = true) {
    const canvas = document.createElement('canvas'); const context = canvas.getContext('2d');
    const fontWeight = bold ? 'Bold ' : ''; context.font = `${fontWeight}${fontSize}px Arial`;

    const lines = [];
    // Ensure text lines are strings for measureText and fillText
    if (textLine1 !== null && textLine1 !== undefined && String(textLine1).trim() !== "") lines.push(String(textLine1).trim());
    if (textLine2 !== null && textLine2 !== undefined && String(textLine2).trim() !== "") lines.push(String(textLine2).trim());
    if (lines.length === 0) lines.push(" ");

    let maxWidth = 0;
    lines.forEach(line => { const metrics = context.measureText(line); if (metrics.width > maxWidth) maxWidth = metrics.width; });

    const lineHeight = fontSize;
    const lineSpacing = lineHeight * 0.3;
    const totalTextHeight = (lines.length * lineHeight) + (Math.max(0, lines.length - 1) * lineSpacing);
    const vPad = fontSize * 0.4; const hPad = fontSize * (bgColor ? 0.3 : 0.05);

    canvas.width = Math.ceil(maxWidth + hPad * 2); canvas.height = Math.ceil(totalTextHeight + vPad * 2);

    if (bgColor) {
        context.fillStyle = bgColor; context.beginPath(); const r = fontSize / 2.5;
        context.moveTo(r, 0); context.lineTo(canvas.width - r, 0); context.quadraticCurveTo(canvas.width, 0, canvas.width, r);
        context.lineTo(canvas.width, canvas.height - r); context.quadraticCurveTo(canvas.width, canvas.height, canvas.width - r, canvas.height);
        context.lineTo(r, canvas.height); context.quadraticCurveTo(0, canvas.height, 0, canvas.height - r);
        context.lineTo(0, r); context.quadraticCurveTo(0, 0, r, 0); context.closePath(); context.fill();
    }

    context.font = `${fontWeight}${fontSize}px Arial`; context.fillStyle = textColor;
    context.textAlign = 'center'; context.textBaseline = 'middle';

    let startY = canvas.height / 2 - totalTextHeight / 2 + lineHeight / 2;

    lines.forEach((line, index) => context.fillText(line, canvas.width / 2, startY + index * (lineHeight + lineSpacing)));

    const texture = new THREE.CanvasTexture(canvas); texture.needsUpdate = true;
    const material = new THREE.SpriteMaterial({ map: texture, transparent: true, sizeAttenuation: true });
    const sprite = new THREE.Sprite(material);

    const desiredSpriteWorldHeight = VISUAL_GRID_SPACING * 0.3 * (lines.length > 1 ? 1.4 : 1.0);
    sprite.scale.set((desiredSpriteWorldHeight * canvas.width) / canvas.height, desiredSpriteWorldHeight, 1);
    sprite.position.copy(position);
    return sprite;
}


function createBoxVisual(boxData) {
    const group = new THREE.Group(); group.userData.boxData = boxData;
    const axisMap = {x: 0, y: 1, z: 2};
    const displayMin = new THREE.Vector3( getDisplayCoord(boxData.minBounds[axisMap.x], 'x'), getDisplayCoord(boxData.minBounds[axisMap.y], 'y'), getDisplayCoord(boxData.minBounds[axisMap.z], 'z') );
    const displayMax = new THREE.Vector3( getDisplayCoord(boxData.maxBounds[axisMap.x], 'x'), getDisplayCoord(boxData.maxBounds[axisMap.y], 'y'), getDisplayCoord(boxData.maxBounds[axisMap.z], 'z') );
    const displayBox3 = new THREE.Box3(displayMin, displayMax);
    const boxHelper = new THREE.Box3Helper(displayBox3, BOX_HELPER_COLOR); group.add(boxHelper);
    const displayCenter = new THREE.Vector3(); displayBox3.getCenter(displayCenter);
    const label = createTextLabel(boxData.text1, boxData.text2, displayCenter, LABEL_FONT_SIZE, LABEL_TEXT_COLOR, LABEL_BG_COLOR);
    group.add(label); return group;
}


function createAxisVisuals() {
    if (axisVisualsGroup) scene.remove(axisVisualsGroup);
    axisVisualsGroup = new THREE.Group(); axisVisualsGroup.name = "AxisVisuals";

    const formatLabel = val => (val === "+∞" ? "+∞" : (val === "-∞" ? "-∞" : val));
    function addAxis(axisChar, color) {
        const boundaries = axisBoundaryMaps[axisChar];
        if (boundaries.length <= 1) return;
        const visualLength = (boundaries.length - 1) * VISUAL_GRID_SPACING;
        const linePoints = [new THREE.Vector3(), new THREE.Vector3()];
        linePoints[1][axisChar] = visualLength;
        axisVisualsGroup.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints(linePoints), new THREE.LineBasicMaterial({color})));
        boundaries.forEach((boundaryValue, index) => {
            const tickPos = new THREE.Vector3();
            tickPos[axisChar] = index * VISUAL_GRID_SPACING;
            const offset = new THREE.Vector3();
            if (axisChar === 'x') { offset.y = -VISUAL_GRID_SPACING*0.2; }
            else if (axisChar === 'y') { offset.x = -VISUAL_GRID_SPACING*0.2; }
            else { offset.x = -VISUAL_GRID_SPACING*0.2; }
            tickPos.add(offset);
            const tickLabel = createTextLabel(formatLabel(boundaryValue), null, tickPos, AXIS_TICK_LABEL_FONT_SIZE, AXIS_TICK_LABEL_COLOR, null, false);
            axisVisualsGroup.add(tickLabel);
        });
        const axisNamePos = new THREE.Vector3();
        axisNamePos[axisChar] = visualLength + VISUAL_GRID_SPACING * 0.5;
        const axisNameLabel = createTextLabel(axisChar.toUpperCase(), null, axisNamePos, AXIS_TICK_LABEL_FONT_SIZE*1.2, color, null, true);
        axisVisualsGroup.add(axisNameLabel);
    }
    addAxis('x', 0xff0000); addAxis('y', 0x00ff00); addAxis('z', 0x0000ff);
    scene.add(axisVisualsGroup);
}


// --- Slice Logic ---
function updateSlice() {
    clearProjectionVisuals();
    const axis = sliceAxisSelect.value;
    const sliceIndex = parseInt(sliceCoordinateSlider.value);
    if (axis === 'none' || !axisBoundaryMaps[axis] || isNaN(sliceIndex) || sliceIndex >= axisBoundaryMaps[axis].length) {
        allVisualsGroup.children.forEach(c => c.visible = true);
        projectionVisualsGroup.visible = false;
        slicePlaneHelper.visible = false;
        sliceAxisIntersectionDot.visible = false;
        if (sliceCoordValueSpan) sliceCoordValueSpan.innerHTML = " ";
        return;
    }
    updateSlicePlaneHelper(axis, sliceIndex);
    const rawSliceValue = axisBoundaryMaps[axis][sliceIndex];
    if (sliceCoordValueSpan) sliceCoordValueSpan.textContent = rawSliceValue;
    const do2DProjection = show2DProjectionCheckbox.checked;
    projectionVisualsGroup.visible = do2DProjection;
    allVisualsGroup.children.forEach(visualGroup => {
        const boxData = visualGroup.userData.boxData;
        if (!boxData) return;
        const axisMap = {x: 0, y: 1, z: 2};
        const minIndex = axisBoundaryMaps[axis].indexOf(boxData.minBounds[axisMap[axis]]);
        const maxIndex = axisBoundaryMaps[axis].indexOf(boxData.maxBounds[axisMap[axis]]);
        const intersects = (minIndex <= sliceIndex && maxIndex >= sliceIndex);
        if (do2DProjection) {
            visualGroup.visible = false;
            if (intersects) create2DProjection(boxData, axis, sliceIndex);
        } else {
            visualGroup.visible = intersects;
        }
    });
}

function clearProjectionVisuals() {
    while (projectionVisualsGroup.children.length > 0) {
        const child = projectionVisualsGroup.children[0];
        projectionVisualsGroup.remove(child);
        if (child.geometry) child.geometry.dispose();
        if (child.material) {
            if (child.material.map) child.material.map.dispose();
            child.material.dispose();
        }
    }
}

function create2DProjection(boxData, axis, sliceIndex) {
    const axisMap = {x: 0, y: 1, z: 2};
    const otherAxes = ['x', 'y', 'z'].filter(a => a !== axis);
    const uAxis = otherAxes[0]; const vAxis = otherAxes[1];
    const displayMinU = getDisplayCoord(boxData.minBounds[axisMap[uAxis]], uAxis);
    const displayMaxU = getDisplayCoord(boxData.maxBounds[axisMap[uAxis]], uAxis);
    const displayMinV = getDisplayCoord(boxData.minBounds[axisMap[vAxis]], vAxis);
    const displayMaxV = getDisplayCoord(boxData.maxBounds[axisMap[vAxis]], vAxis);
    const displaySliceCoord = getDisplayCoord(axisBoundaryMaps[axis][sliceIndex], axis);
    const outlinePoints = []; const p1 = new THREE.Vector3();
    p1[uAxis] = displayMinU; p1[vAxis] = displayMinV; p1[axis] = displaySliceCoord;
    const p2 = new THREE.Vector3(); p2[uAxis] = displayMaxU; p2[vAxis] = displayMinV; p2[axis] = displaySliceCoord;
    const p3 = new THREE.Vector3(); p3[uAxis] = displayMaxU; p3[vAxis] = displayMaxV; p3[axis] = displaySliceCoord;
    const p4 = new THREE.Vector3(); p4[uAxis] = displayMinU; p4[vAxis] = displayMaxV; p4[axis] = displaySliceCoord;
    outlinePoints.push(p1,p2,p3,p4);
    const outlineGeom = new THREE.BufferGeometry().setFromPoints(outlinePoints);
    const outlineMat = new THREE.LineBasicMaterial({ color: PROJECTION_OUTLINE_COLOR });
    const outline = new THREE.LineLoop(outlineGeom, outlineMat);
    outline.renderOrder = 1; projectionVisualsGroup.add(outline);
    const worldCenter = new THREE.Vector3(); new THREE.Box3().setFromPoints(outlinePoints).getCenter(worldCenter);
    worldCenter[axis] += 0.02;
    const projLabel = createTextLabel(boxData.text1, boxData.text2, worldCenter, LABEL_FONT_SIZE, PROJECTION_OUTLINE_COLOR, null, false );
    projLabel.renderOrder = 2; projectionVisualsGroup.add(projLabel);
}

function updateSlicePlaneHelper(axis, sliceIndex) {
    const isHelperVisible = (showSlicePlaneCheckbox.checked || show2DProjectionCheckbox.checked) && axis !== 'none';
    slicePlaneHelper.visible = isHelperVisible;
    sliceAxisIntersectionDot.visible = isHelperVisible;
    if (!isHelperVisible) return;

    const displayCoord = sliceIndex * VISUAL_GRID_SPACING;
    const vizSizeX = (axisBoundaryMaps.x.length - 1) * VISUAL_GRID_SPACING;
    const vizSizeY = (axisBoundaryMaps.y.length - 1) * VISUAL_GRID_SPACING;
    const vizSizeZ = (axisBoundaryMaps.z.length - 1) * VISUAL_GRID_SPACING;
    let planeWidth, planeHeight;
    if (axis === 'x') { planeWidth = vizSizeY; planeHeight = vizSizeZ; }
    else if (axis === 'y') { planeWidth = vizSizeX; planeHeight = vizSizeZ; }
    else { planeWidth = vizSizeX; planeHeight = vizSizeY; }
    if (slicePlaneHelper.geometry.parameters.width !== planeWidth + VISUAL_GRID_SPACING || slicePlaneHelper.geometry.parameters.height !== planeHeight + VISUAL_GRID_SPACING) {
        if (slicePlaneHelper.geometry) slicePlaneHelper.geometry.dispose();
        slicePlaneHelper.geometry = new THREE.PlaneGeometry(planeWidth + VISUAL_GRID_SPACING, planeHeight + VISUAL_GRID_SPACING);
    }
    const helperPosition = new THREE.Vector3(vizSizeX/2, vizSizeY/2, vizSizeZ/2);
    helperPosition[axis] = displayCoord;
    slicePlaneHelper.position.copy(helperPosition);
    slicePlaneHelper.rotation.set(0,0,0);
    if (axis === 'x') slicePlaneHelper.rotation.y = Math.PI / 2;
    else if (axis === 'y') slicePlaneHelper.rotation.x = Math.PI / 2;
    const dotPosition = new THREE.Vector3();
    dotPosition[axis] = displayCoord;
    sliceAxisIntersectionDot.position.copy(dotPosition);
}

function updateSliderRange() {
    const axis = sliceAxisSelect.value;
    if (axis === 'none' || !axisBoundaryMaps[axis] || axisBoundaryMaps[axis].length <= 1) {
        sliceCoordinateSlider.disabled = true; sliceCoordinateSlider.min = 0; sliceCoordinateSlider.max = 0;
    } else {
        sliceCoordinateSlider.disabled = false;
        sliceCoordinateSlider.min = 0;
        sliceCoordinateSlider.max = axisBoundaryMaps[axis].length - 1;
        sliceCoordinateSlider.step = 1;
        if (parseInt(sliceCoordinateSlider.value) > sliceCoordinateSlider.max) sliceCoordinateSlider.value = sliceCoordinateSlider.max;
    }
}

// --- Mouse Dragging Logic for Ordinal Slicing ---
function onDocumentMouseDown(event) {
    event.preventDefault();
    mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
    mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;
    raycaster.setFromCamera(mouse, camera);

    const currentSliceAxis = sliceAxisSelect.value;
    if (currentSliceAxis === 'none' || !slicePlaneHelper.visible) return;

    const intersects = raycaster.intersectObject(slicePlaneHelper, false);

    if (intersects.length > 0) {
        isDraggingSlicePlane = true;
        controls.enabled = false;
        renderer.domElement.style.cursor = 'grabbing';

        dragActiveAxis = currentSliceAxis;
        dragInitialIndex = parseInt(sliceCoordinateSlider.value);
        dragStartPointOnHelper.copy(intersects[0].point); // World space intersection

        camera.getWorldDirection(dragPlane.normal);
        dragPlane.setFromNormalAndCoplanarPoint(dragPlane.normal, dragStartPointOnHelper);
    }
}

function onDocumentMouseMove(event) {
    event.preventDefault();
    mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
    mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

    if (!isDraggingSlicePlane && sliceAxisSelect.value !== 'none' && slicePlaneHelper.visible) {
        raycaster.setFromCamera(mouse, camera);
        const intersects = raycaster.intersectObject(slicePlaneHelper, false);
        renderer.domElement.style.cursor = intersects.length > 0 ? 'grab' : 'default';
    }

    if (!isDraggingSlicePlane) return;

    raycaster.setFromCamera(mouse, camera);
    if (raycaster.ray.intersectPlane(dragPlane, planeIntersectPoint)) {
        const dragVector = planeIntersectPoint.clone().sub(dragStartPointOnHelper);
        let displacement = 0;
        if (dragActiveAxis === 'x') displacement = dragVector.x;
        else if (dragActiveAxis === 'y') displacement = dragVector.y;
        else if (dragActiveAxis === 'z') displacement = dragVector.z;

        // Convert continuous displacement to discrete index change
        const indexChange = Math.round(displacement / VISUAL_GRID_SPACING);

        const sliderMax = parseInt(sliceCoordinateSlider.max);
        const sliderMin = parseInt(sliceCoordinateSlider.min);
        let newIndex = dragInitialIndex + indexChange;

        // Clamp the new index to the slider's bounds
        newIndex = Math.max(sliderMin, Math.min(sliderMax, newIndex));

        if (newIndex !== parseInt(sliceCoordinateSlider.value)) {
            sliceCoordinateSlider.value = newIndex;
            updateSlice();
        }
    }
}

function onDocumentMouseUp(event) {
    if (isDraggingSlicePlane) {
        isDraggingSlicePlane = false;
        controls.enabled = true;
        renderer.domElement.style.cursor = (sliceAxisSelect.value !== 'none' && slicePlaneHelper.visible) ? 'grab' : 'default';
    }
}

// --- Animation Loop & Resize ---
function animate() { requestAnimationFrame(animate); if(controls.enableDamping) controls.update(); renderer.render(scene, camera); }
function onWindowResize() {
    camera.aspect = window.innerWidth / window.innerHeight; camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
}