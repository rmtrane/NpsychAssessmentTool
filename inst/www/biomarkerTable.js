let activeHoverBox = null;
let activeCell = null;
let activeIcon = null; // Track the active plot icon

function toggleHoverBox(event, cellId) {
  event.stopPropagation();

  let hoverBox = document.getElementById('hoverBox_' + cellId);
  let clickedCell = event.currentTarget.closest('.flex-cell') || event.currentTarget.closest('td');
  let clickedIcon = event.currentTarget.closest('.plot-icon') || event.currentTarget;

  // If clicking the same cell, toggle off
  if (activeHoverBox === hoverBox && hoverBox.classList.contains('active')) {
    hoverBox.classList.remove('active');
    activeHoverBox = null;
    if (activeCell) {
      activeCell.style.pointerEvents = '';
      activeCell = null;
    }
    if (activeIcon) {
      activeIcon.classList.remove('no-tooltip');
      activeIcon = null;
    }
    return;
  }

  // Hide any currently active hover box
  if (activeHoverBox) {
    activeHoverBox.classList.remove('active');
    if (activeCell) {
      activeCell.style.pointerEvents = '';
    }
    if (activeIcon) {
      activeIcon.classList.remove('no-tooltip');
    }
  }

  // Disable hover on the clicked cell
  if (clickedCell) {
    clickedCell.style.pointerEvents = 'none';
    activeCell = clickedCell;
  }
  if (clickedIcon) {
    clickedIcon.classList.add('no-tooltip');
    activeIcon = clickedIcon;
  }

  // Reset positioning
  hoverBox.style.top = '';
  hoverBox.style.bottom = '';
  hoverBox.style.left = '';
  hoverBox.style.right = '';
  hoverBox.style.marginTop = '';
  hoverBox.style.marginBottom = '';

  // Temporarily show to measure
  hoverBox.style.visibility = 'hidden';
  hoverBox.style.display = 'block';
  
  // Get dimensions and positions
  const cellRect = clickedCell.getBoundingClientRect();
  const tableRect = clickedCell.closest('.biomarkerTable').getBoundingClientRect();
  const hoverBoxWidth = hoverBox.offsetWidth;
  const hoverBoxHeight = hoverBox.offsetHeight;
  
  // Calculate available space
  const spaceBelow = window.innerHeight - cellRect.bottom;
  const spaceAbove = cellRect.top - tableRect.top;
  const spaceRightInTable = tableRect.right - cellRect.left;  // Space from cell to right edge of table
  const spaceLeftInTable = cellRect.right - tableRect.left;   // Space from left edge of table to cell
  
  // Determine best position
  let position = {};
  
  // Vertical positioning (keep as is)
  if (spaceBelow >= hoverBoxHeight + 10) {
    position.top = '100%';
    position.marginTop = '5px';
  } else if (spaceAbove >= hoverBoxHeight + 10) {
    position.bottom = '100%';
    position.marginBottom = '5px';
  } else {
    if (cellRect.top > window.innerHeight / 2) {
      const topOffset = window.pageYOffset - cellRect.top + 10;
      position.top = topOffset + 'px';
    } else {
      const topOffset = Math.min(0, window.innerHeight - cellRect.bottom - hoverBoxHeight - 10);
      position.top = '100%';
      position.marginTop = topOffset + 'px';
    }
  }
  
  // Horizontal positioning - keep within table bounds
  if (spaceRightInTable >= hoverBoxWidth) {
    // Enough space on the right within table
    position.left = '0';
  } else if (spaceLeftInTable >= hoverBoxWidth) {
    // Not enough space on right, position to the left
    position.right = '0';
    position.left = 'auto';
  } else {
    // Hover box is wider than available space
    // Position it to align with the right edge of the table
    const offsetFromCellLeft = tableRect.right - cellRect.left - hoverBoxWidth - 10;
    if (offsetFromCellLeft < 0) {
      // If hover box is wider than table, align with left edge of table
      const leftOffset = tableRect.left - cellRect.left + 10;
      position.left = leftOffset + 'px';
    } else {
      position.left = offsetFromCellLeft + 'px';
    }
  }
  
  // Reset visibility and apply positioning
  hoverBox.style.display = '';
  hoverBox.style.visibility = '';
  
  // Apply calculated positions
  Object.keys(position).forEach(key => {
    hoverBox.style[key] = position[key];
  });

  // Show the hover box
  hoverBox.classList.add('active');
  hoverBox.style.pointerEvents = 'auto';
  activeHoverBox = hoverBox;
}

// Close hover box when clicking outside - with cell pointer events handling
document.addEventListener('click', function (event) {
  if (!activeHoverBox) return;

  // Check if the click is within the hover box or its children
  if (activeHoverBox.contains(event.target)) {
    return; // Don't close if clicking inside the hover box
  }

  // Check if clicking on the trigger element (plot icon)
  if (event.target.closest('.plot-icon') || event.target.closest('.clickable-cell')) {
    return; // Let the toggleHoverBox handle this
  }

  // Re-enable hover on the active cell when closing
  if (activeCell) {
    activeCell.style.pointerEvents = '';
    activeCell = null;
  }
  if (activeIcon) {
    activeIcon.classList.remove('no-tooltip');
    activeIcon = null;
  }

  // Close the hover box for clicks outside
  activeHoverBox.classList.remove('active');
  activeHoverBox = null;
});

// Prevent clicks within hover box from bubbling up
document.addEventListener('DOMContentLoaded', function () {
  // Add event listeners to all hover boxes to prevent click propagation
  document.querySelectorAll('.hover-box').forEach(box => {
    box.addEventListener('click', function (event) {
      event.stopPropagation();
    });
  });
});

// Close hover box with Escape key
document.addEventListener('keydown', function (event) {
  if (event.key === 'Escape' && activeHoverBox) {
    // Re-enable hover on the active cell
    if (activeCell) {
      activeCell.style.pointerEvents = '';
      activeCell = null;
    }
    if (activeIcon) {
      activeIcon.classList.remove('no-tooltip');
      activeIcon = null;
    }
    activeHoverBox.classList.remove('active');
    activeHoverBox = null;
  }
});