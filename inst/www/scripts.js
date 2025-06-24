// Show spinner. When shiny is busy, change display to 'block'
$(document).on('shiny:busy', function(event) {
  document.getElementById('spinner_overlay').style.display = "block";
  document.getElementById('spinner').style.display = 'block';
})

// Show spinner. When shiny is busy, change display to 'none'
$(document).on('shiny:idle', function(event) {
  document.getElementById('spinner_overlay').style.display = "none";
  document.getElementById('spinner').style.display = 'none';
})

// Custom message handler to click virtual buttons
Shiny.addCustomMessageHandler("click", (message) => {
  Shiny.setInputValue(message, "click", {priority: "event"});
})

// Custom Message Handler to set input value
Shiny.addCustomMessageHandler("setInputValue", (message) => {
  // console.log("setInputValue!!!");
  console.log("Setting input " + message.inputId + " to " + message.inputValue);
  if (message.priority) {
    Shiny.setInputValue(message.inputId, message.inputValue, {priority: message.priority});
  } else {
    Shiny.setInputValue(message.inputId, message.inputValue);
  }
})

// Function to resize dropdown menu so that the width is determined by the longest 
// element in the list.
function resizeSelectize(id) {
  // console.log('Here...');
  var selectize = $('#' + id)[0].selectize;
  if (selectize) {
    var longest = '';
    selectize.options && Object.values(selectize.options).forEach(function(opt) {
      if (opt.label.length > longest.length) {
        longest = opt.label;
      }
    });

    var temp = document.createElement('span');
    temp.style.visibility = 'hidden';
    temp.style.position = 'absolute';
    temp.style.whiteSpace = 'nowrap';
    temp.style.font = window.getComputedStyle($('.selectize-input')[0]).font;
    temp.innerText = longest;
    document.body.appendChild(temp);
    var width = temp.offsetWidth + 50; // padding for dropdown arrow
    document.body.removeChild(temp);

    // Apply width to the selectize control
    $('#' + id)[0].selectize.$control.css('width', width + 'px');
  }
}

// Remove empty raw_suffix columns, and make raw column span both
function removeRawSuffix() {
  var outputElement = document.getElementById("main_table-mainTable");

    if (outputElement.length == 0) {
      console.log("return 1");
      return;
    }

    var tableElement = outputElement.getElementsByClassName("gt_table");

    if (tableElement.length == 0) {
      console.log("return 2");
      return;
    } 
      
    var tableElement0 = outputElement.getElementsByClassName("gt_table")[0];
    
    var rows = tableElement0.getElementsByTagName("tr");

    if (rows.length == 0) {
      console.log("return 3");
      return;
    }

    for (let row of rows) {
      var raw_suffix_removed = false;
      var row_children = row.children;

      for (let row_element of row_children) {
        if (row_element.headers.split(" ").includes("raw_suffix") && 
          row_element.textContent == "") {
            // console.log("Removing raw_suffix!");
            row_element.remove();
            var raw_suffix_removed = true;
        }
      }
      
      if (raw_suffix_removed) {
        for (let row_element of row_children) {
          if (row_element.headers.split(" ").includes("raw")) {
            // console.log("Adjusting raw column");
            row_element.colSpan = "2";
            row_element.style.textAlign = "center";
          }
        }
      }
    }
}

// Custom Message Handler to run the function above. 
Shiny.addCustomMessageHandler("removeRawSuffix", (message) => {
  removeRawSuffix();
})


// Enable hitting enter after password input
$(document).keyup(function(event) {
  if ($("[id$=api_token]").is(":focus") && event.key == "Enter") {
    $("[id$=fetch_data_button]").click();
  }
});

// Add listeners and callbacks for color inputs in the descriptions/fill_values 
// table under "Options"
function addEventListenersToColors(id) {
  [].forEach.call($("[type=color]"), function(v,i,a) {
    v.addEventListener("input", function(event) {
      var shinyInputVar = id + "-" + event.target.id;

      // console.log(shinyInputVar);

      Shiny.setInputValue(shinyInputVar, event.target.value);
      Shiny.setInputValue(id + "-newColorPicked", event.target.id, {priority: "event"});
    })
  })
}

// Function to call when base plots are plotted. This includes re-plotting after restyling and relayout.
// We use this to keep track of visibility of traces
function afterPlot (x, input) {
  // Array to hold info on all traces
  var out = [];

  // inputName to use in Shiny
  var inputName = input.ns + '-' + input.name

  // Function to get needed info from traces.
  function getTraceInfo(trace, traceindex) {
    // If trace has a name, set tracename
    if (typeof trace.name !== 'undefined') {
      var tracename = trace.name ;
    } else {
      var tracename = '';
    }

    // If trace has visible attribute, set tracevisible
    if (typeof trace.visible !== 'undefined') {
      var tracevisible = trace.visible ;
    } else {
      var tracevisible = '';
    }

    // If trace has customdata attribute, set name and create
    // input$ns-name_visibility in R session
    if (typeof trace.customdata !== 'undefined') {
      var name = trace.customdata ;

      Shiny.setInputValue(input.ns + '-' + name + '_visibility', tracevisible);
    } else {
      var name = '';
    }

    // Add to out list
    out.push([tracename=tracename, index=traceindex, name = name, visible = tracevisible]);
  }

  // Run function for each trace
  x.data.forEach(getTraceInfo);

  // Create input$input.name. This holds all traces with name, index, and visibility.
  Shiny.setInputValue(inputName, out);
}
