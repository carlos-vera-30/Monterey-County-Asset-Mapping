
```{r, echo=FALSE, results='asis'}
cat('
<script>
// Global variables
let RCategoryColors = {};
let allAssets = [];
let filteredAssets = [];
let searchCoords = null;
let userMarker = null;

function initializeInterface() {
  console.log("Initializing interface...");
  try {
    // Check if data is available from the injected variables
    if (typeof assets !== "undefined" && assets.length > 0) {
      allAssets = assets;
      console.log("Assets loaded:", allAssets.length);
    } else {
      console.error("No assets data found");
      showNotification("No asset data loaded. Map may not function correctly.", "error");
      return;
    }
    
    if (typeof categoryColors !== "undefined" && Object.keys(categoryColors).length > 0) {
      RCategoryColors = categoryColors;
      console.log("Category colors loaded:", Object.keys(RCategoryColors).length);
    } else {
      console.warn("No category colors found, creating default mapping");
      createDefaultColorMapping();
    }

    // Ensure each asset has a color property
    allAssets.forEach((asset, index) => {
      if (!asset.color || asset.color === "#000000" || asset.color === "black") {
        asset.color = getAssetColor(asset.Asset_Category);
      }
    });

    // Initialize filtered assets
    filteredAssets = [...allAssets];

    // Set up the interface components
    populateCategoryFilter();
    updateLegend();
    updateMapMarkers();
    updateResultsTable();

    // Add event listeners
    setupEventListeners();

    // Initialize DataTable with delay to ensure DOM is ready
    setTimeout(initializeDataTable, 1000);
    
    console.log("Interface initialized successfully");
  } catch (error) {
    console.error("Error initializing interface:", error);
    showNotification("Error initializing the map interface.", "error", 10000);
  }
}

function setupEventListeners() {
  const searchInput = document.getElementById("searchAddress");
  if (searchInput) {
    searchInput.addEventListener("keypress", function(e) {
      if (e.key === "Enter") {
        searchByAddress();
      }
    });
  }

  const distanceFilter = document.getElementById("distanceFilter");
  if (distanceFilter) {
    distanceFilter.addEventListener("change", updateDistanceFilter);
  }
}

function initializeDataTable() {
  try {
    if ($.fn.DataTable && $("#assetsDataTable").length > 0) {
      if ($.fn.DataTable.isDataTable("#assetsDataTable")) {
        $("#assetsDataTable").DataTable().destroy();
      }
      
      window.assetsDataTable = $("#assetsDataTable").DataTable({
        responsive: true,
        pageLength: 10,
        order: [[0, "asc"]],
        columnDefs: [
          { targets: [5], orderable: false }
        ]
      });
    }
  } catch (error) {
    console.warn("DataTable initialization failed:", error);
  }
}

function populateCategoryFilter() {
  const container = document.getElementById("categoryCheckboxes");
  if (!container) {
    console.warn("categoryCheckboxes container not found");
    return;
  }
  
  container.innerHTML = "";
  
  const categories = [...new Set(allAssets.map(asset => asset.Asset_Category))]
    .filter(cat => cat)
    .sort();
  
  if (categories.length === 0) {
    container.innerHTML = "<p>No categories available</p>";
    return;
  }

  console.log("Creating category filters for:", categories);
  
  categories.forEach(category => {
    const id = `cat-${category.replace(/\\s+/g, "_").replace(/[^a-zA-Z0-9_]/g, "")}`;
    const color = getAssetColor(category);
    
    const checkboxDiv = document.createElement("div");
    checkboxDiv.className = "legend-item category-filter-item";
    
    const checkbox = document.createElement("input");
    checkbox.type = "checkbox";
    checkbox.className = "category-check";
    checkbox.id = id;
    checkbox.value = category;
    checkbox.checked = true;
    
    const label = document.createElement("label");
    label.htmlFor = id;
    
    const colorDiv = document.createElement("div");
    colorDiv.className = "legend-color";
    colorDiv.style.backgroundColor = color;
    
    const textSpan = document.createElement("span");
    textSpan.textContent = category;
    
    label.appendChild(colorDiv);
    label.appendChild(textSpan);
    checkboxDiv.appendChild(checkbox);
    checkboxDiv.appendChild(label);
    container.appendChild(checkboxDiv);
    
    checkbox.addEventListener("change", applyFilters);
  });
}

function selectAllCategories() {
  document.querySelectorAll(".category-check").forEach(cb => cb.checked = true);
  applyFilters();
  showNotification("All categories selected", "success", 2000);
}

function clearAllCategories() {
  document.querySelectorAll(".category-check").forEach(cb => cb.checked = false);
  applyFilters();
  showNotification("All categories cleared", "info", 2000);
}

function createDefaultColorMapping() {
  const defaultColors = [
    "#1E88E5", "#4CAF50", "#FF7043", "#8E24AA", "#E91E63", "#F44336", 
    "#29B6F6", "#C62828", "#42A5F5", "#2E7D32", "#FDD835", "#7B1FA2",
    "#66BB6A", "#3F51B5", "#E53935", "#FB8C00", "#757575", "#424242"
  ];
  
  const categories = [...new Set(allAssets.map(asset => asset.Asset_Category))].filter(cat => cat);
  
  categories.forEach((category, index) => {
    RCategoryColors[category] = defaultColors[index % defaultColors.length];
  });
}

function getAssetColor(category) {
  if (!category) {
    return "#607D8B";
  }
  
  let color = RCategoryColors[category];
  
  if (!color) {
    const categoryKey = Object.keys(RCategoryColors).find(key => 
      key.toLowerCase() === category.toLowerCase()
    );
    if (categoryKey) {
      color = RCategoryColors[categoryKey];
    }
  }
  
  if (!color || color === "#000000" || color === "black") {
    color = "#607D8B";
  }
  
  return color;
}

function applyFilters() {
  const selectedCategories = Array.from(document.querySelectorAll(".category-check:checked"))
    .map(checkbox => checkbox.value);

  filteredAssets = selectedCategories.length === 0 ? [] : 
    allAssets.filter(asset => selectedCategories.includes(asset.Asset_Category));

  if (searchCoords) {
    applyDistanceFilter();
  } else {
    filteredAssets.sort((a, b) => (a.Asset_Name || "").localeCompare(b.Asset_Name || ""));
  }

  updateMapMarkers();
  updateLegend();
  updateResultsTable();
}

function applyDistanceFilter() {
  if (!searchCoords || !filteredAssets) return;

  const maxDistance = parseFloat(document.getElementById("distanceFilter")?.value || 999);

  if (maxDistance < 999) {
    filteredAssets = filteredAssets.filter(asset => {
      if (!asset.Latitude || !asset.Longitude) return false;
      const distance = calculateDistance(
        searchCoords.lat, searchCoords.lng,
        asset.Latitude, asset.Longitude
      );
      return distance <= maxDistance;
    });
  }

  filteredAssets.sort((a, b) => {
    if (!a.Latitude || !b.Latitude) return 0;
    const distA = calculateDistance(searchCoords.lat, searchCoords.lng, a.Latitude, a.Longitude);
    const distB = calculateDistance(searchCoords.lat, searchCoords.lng, b.Latitude, b.Longitude);
    return distA - distB;
  });
}

function updateDistanceFilter() {
  if (!searchCoords) {
    showNotification("Please set a location first to filter by distance.", "info");
    return;
  }
  
  applyFilters();

  if (window.assetMap) {
    const distance = parseFloat(document.getElementById("distanceFilter")?.value || 5);
    let zoom = 10;
    if (distance <= 1) zoom = 14;
    else if (distance <= 2) zoom = 13;
    else if (distance <= 5) zoom = 12;
    else if (distance <= 10) zoom = 11;


    window.assetMap.setView([searchCoords.lat, searchCoords.lng], zoom);
  }
}

function updateMapMarkers() {
  if (!window.assetMap) {
    console.warn("assetMap not available");
    return;
  }

  window.assetMap.eachLayer(layer => {
    if (layer instanceof L.CircleMarker) {
      window.assetMap.removeLayer(layer);
    }
  });

  filteredAssets.forEach(asset => {
    if (asset.Latitude && asset.Longitude) {
      const markerColor = asset.color || getAssetColor(asset.Asset_Category);
      
      let popupContent = `
        <div style="min-width: 200px;">
          <strong>${asset.Asset_Name || "N/A"}</strong><br>
          <em style="color: ${markerColor};">${asset.Asset_Category || "N/A"}</em><br>
          <strong>Address:</strong> ${asset.address || "N/A"}<br>
      `;
      
      if (asset.Phone) popupContent += `<strong>Phone:</strong> ${asset.Phone}<br>`;
      if (asset.Email) popupContent += `<strong>Email:</strong> ${asset.Email}<br>`;
      if (asset.Website) {
        const website = asset.Website.startsWith("http") ? asset.Website : "http://" + asset.Website;
        popupContent += `<strong>Website:</strong> <a href="${website}" target="_blank" rel="noopener">Visit Site</a><br>`;
      }

      if (searchCoords) {
        const distance = calculateDistance(searchCoords.lat, searchCoords.lng, asset.Latitude, asset.Longitude);
        if (isFinite(distance)) {
          popupContent += `<strong>Distance:</strong> ${distance.toFixed(2)} miles`;
        }
      }
      
      popupContent += "</div>";
      
      L.circleMarker([asset.Latitude, asset.Longitude], {
        radius: 8,
        fillColor: markerColor,
        color: "white",
        weight: 2,
        opacity: 1,
        fillOpacity: 0.8
      }).bindPopup(popupContent).addTo(window.assetMap);
    }
  });
}

function updateLegend() {
  const legendDiv = document.getElementById("legend");
  if (!legendDiv) return;

  const displayedCategories = [...new Set(filteredAssets.map(asset => asset.Asset_Category))]
    .filter(cat => cat)
    .sort();

  legendDiv.innerHTML = "";
  
  if (displayedCategories.length === 0) {
    legendDiv.innerHTML = "<p style=\"font-style: italic; color: #666;\">No categories selected.</p>";
    return;
  }
  
  displayedCategories.forEach(category => {
    const color = getAssetColor(category);
    const item = document.createElement("div");
    item.className = "legend-item";
    item.innerHTML = `
      <div class="legend-color" style="background-color: ${color};"></div>
      ${category}
    `;
    legendDiv.appendChild(item);
  });
}

function updateResultsTable() {
  if (!window.assetsDataTable) {
    console.warn("DataTable not available");
    return;
  }

  try {
    const tableData = filteredAssets.map(asset => {
      let distanceText = "N/A";
      if (searchCoords && asset.Latitude && asset.Longitude) {
        const dist = calculateDistance(searchCoords.lat, searchCoords.lng, asset.Latitude, asset.Longitude);
        if (isFinite(dist)) {
          distanceText = `${dist.toFixed(2)} miles`;
        }
      }

      const websiteLink = asset.Website ? 
        `<a href="${asset.Website.startsWith("http") ? asset.Website : "http://" + asset.Website}" target="_blank" rel="noopener">${asset.Website}</a>` : 
        "";

      return [
        asset.Asset_Name || "",
        asset.Asset_Category || "",
        asset.address || "",
        asset.Phone || "",
        asset.Email || "",
        websiteLink,
        distanceText
      ];
    });

    window.assetsDataTable.clear();
    window.assetsDataTable.rows.add(tableData).draw();
  } catch (error) {
    console.error("Error updating results table:", error);
  }
}

function calculateDistance(lat1, lon1, lat2, lon2) {
  if (lat1 == null || lon1 == null || lat2 == null || lon2 == null) return Infinity;
  const R = 3959;
  const dLat = (lat2 - lat1) * Math.PI / 180;
  const dLon = (lon2 - lon1) * Math.PI / 180;
  const a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
    Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
    Math.sin(dLon / 2) * Math.sin(dLon / 2);
  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
  return R * c;
}

async function searchByAddress() {
  const address = document.getElementById("searchAddress")?.value.trim();
  if (!address) {
    showNotification("Please enter an address.", "error");
    return;
  }
  
  showNotification("Searching for address...", "info", 2000);
  
  try {
    const fullAddressQuery = address.includes(",") ? address : `${address}, Monterey County, CA`;
    const response = await fetch(`https://nominatim.openstreetmap.org/search?format=json&q=${encodeURIComponent(fullAddressQuery)}&limit=1`);
    
    if (!response.ok) throw new Error(`Geocoding service returned status ${response.status}`);
    const data = await response.json();

    if (data && data.length > 0) {
      const coords = {
        lat: parseFloat(data[0].lat),
        lng: parseFloat(data[0].lon)
      };
      setSearchLocation(coords);
      showNotification("Address found! Showing nearby resources.", "success");
    } else {
      showNotification("Address not found. Please try a more specific address.", "error");
    }
  } catch (error) {
    console.error("Geocoding error:", error);
    showNotification("Error searching for address. Please try again.", "error");
  }
}

function getCurrentLocation() {
  if (!navigator.geolocation) {
    showNotification("Geolocation is not supported by your browser.", "error");
    return;
  }
  
  showNotification("Getting your current location...", "info", 3000);
  
  navigator.geolocation.getCurrentPosition(
    position => {
      const coords = {
        lat: position.coords.latitude,
        lng: position.coords.longitude
      };
      setSearchLocation(coords);
      showNotification("Using your current location.", "success");
    },
    error => {
      console.error("Geolocation error:", error);
      let msg = "Could not get your location. ";
      if (error.code === 1) msg += "Permission denied.";
      else if (error.code === 2) msg += "Position unavailable.";
      else msg += "Timeout.";
      showNotification(msg, "error");
    },
    { 
      enableHighAccuracy: true,
      timeout: 10000,
      maximumAge: 0
    }
  );
}

function setSearchLocation(coords) {
  searchCoords = coords;
  
  const distanceFilterDiv = document.getElementById("distanceFilterDiv");
  if (distanceFilterDiv) {
    distanceFilterDiv.style.display = "block";
  }

  if (window.assetMap) {
    if (userMarker) {
      window.assetMap.removeLayer(userMarker);
    }
    
    userMarker = L.marker([coords.lat, coords.lng], {
      icon: L.icon({
        iconUrl: "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.7.1/images/marker-icon-2x-red.png",
        iconSize: [25, 41],
        iconAnchor: [12, 41],
        popupAnchor: [1, -34],
        shadowUrl: "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.7.1/images/marker-shadow.png",
        shadowSize: [41, 41]
      })
    }).addTo(window.assetMap);
    
    userMarker.bindPopup("Your Searched Location").openPopup();
    updateDistanceFilter();
  }
}

function resetMap() {
  searchCoords = null;
  
  const searchInput = document.getElementById("searchAddress");
  if (searchInput) searchInput.value = "";
  
  const distanceFilterDiv = document.getElementById("distanceFilterDiv");
  if (distanceFilterDiv) distanceFilterDiv.style.display = "none";
  
  const distanceFilter = document.getElementById("distanceFilter");
  if (distanceFilter) distanceFilter.value = "5";

  if (userMarker && window.assetMap) {
    window.assetMap.removeLayer(userMarker);
    userMarker = null;
  }

  if (window.assetMap) {
    window.assetMap.setView([36.6777, -121.6555], 10);
  }

  selectAllCategories();
  showNotification("Map and filters reset.", "success");
}

function showNotification(message, type = "info", duration = 4000) {
  const notificationsDiv = document.getElementById("notifications");
  if (!notificationsDiv) {
    console.log("Notification:", message);
    return;
  }

  notificationsDiv.innerHTML = "";

  const notification = document.createElement("div");
  notification.className = `notification ${type}`;
  notification.textContent = message;
  notification.style.display = "block";

  notificationsDiv.appendChild(notification);

  if (duration > 0) {
    setTimeout(() => {
      notification.style.opacity = "0";
      setTimeout(() => {
        if (notification.parentNode) {
          notification.parentNode.removeChild(notification);
        }
      }, 500);
    }, duration);
  }
}

window.addEventListener("resize", function() {
  if (window.assetMap) {
    setTimeout(function() {
      window.assetMap.invalidateSize();
    }, 100);
  }
});

// Make functions available globally
window.selectAllCategories = selectAllCategories;
window.clearAllCategories = clearAllCategories;
window.searchByAddress = searchByAddress;
window.getCurrentLocation = getCurrentLocation;
window.updateDistanceFilter = updateDistanceFilter;
window.resetMap = resetMap;

// Initialize when DOM is ready
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", function() {
    setTimeout(initializeInterface, 500);
  });
} else {
  setTimeout(initializeInterface, 500);
}
</script>
')
```