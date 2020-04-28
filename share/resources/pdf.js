
(function($) {
	version = '2.3.200';
	
	const worker_url = '//cdn.jsdelivr.net/npm/pdfjs-dist@'+version+'/build/pdf.worker.js';
	
	
	libs = [
	  			'https://cdn.jsdelivr.net/npm/pdfjs-dist@'+version+'/build/pdf.min.js',
					'https://cdnjs.cloudflare.com/ajax/libs/pdf.js/'+version+'/pdf.min.js'
	];
	
	var pdfjsLib;
	
	window.pdf = {
		start: function() {
			const p = new Promise(function(resolve, reject) {
				jQuery.ajax({"url": (libs[0]), "dataType": "script"})
          .then(function() {
				    console.log('pdjjs loaded');
					  window.pdf.init();
					  resolve();
				});
			});
			return p;
		},
		init: function() {
			pdfjsLib = window['pdfjs-dist/build/pdf'];
			// The workerSrc property shall be specified.
			pdfjsLib.GlobalWorkerOptions.workerSrc = worker_url;
			console.log('init done');
		},
		load: function(element, url, opts) {
			
			const promise = new Promise(function(resolve, reject) {
			
				// Asynchronous download of PDF
				const loadingTask = pdfjsLib.getDocument(url);
			
				 loadingTask.promise.then(function(pdf) {
				  console.log('PDF loaded');
				  
				  viewer(element, pdf, opts).then(function() {
					  resolve();  
				  });
				  
				}, function (reason) {
				  // PDF loading error
				  reject();
				  console.error(reason);
				});
				
			});
			return promise;
		}
	};

	
	function viewer(element, pdf, opts) {
			
			opts = opts || {};
			
			const $e = $(element);
			
			const $h = $('<div class="pdf-toolbox"/>');
			
			$e.append($h);
			
			const $viewer = $('<div style="width:100%" class="pdf-viewer">');
			
			if(opts.width) {
				$viewer.css({'width': opts.width, 'height': opts.height, 'overflow': 'auto'});
			}
			
			$e.append($viewer);
			
			const $canvas = $('<canvas>');
			const canvas = $canvas.get(0);
			$viewer.append($canvas);
			
			var pageNumber = 1;
		    var scale = typeof(opts.scale) != 'undefined' ? opts.scale : 1;
			var curPage;
			
			$e.data('pdf', pdf);
			$e.data('viewer', this);
			
			function renderPage(page) {
		        curPage = page;
				var viewport = page.getViewport(scale);
		        var context = canvas.getContext('2d');
		        
		        canvas.height = viewport.height;
		        canvas.width = viewport.width;
		        var renderContext = {
		           canvasContext: context,
		           viewport: viewport
		        };
		        return page.render(renderContext);
		     }
		
		     function displayPage() {
		        return pdf.getPage(pageNumber).then(function (page) { 
		        	renderPage(page); 
		        });
		     }
		     
		     function _b(icon) {
		    	 const $b =  $('<span class="btn btn-xs btn-light">'+icon+'</span>');
		    	 $h.append($b);
		    	 return $b;
		     }
		     
		     var $b;
		     
		     
		     $b = _b('-');
		     
		     $b.on('click', function() {
		    	 if (scale <= 0.25) {
		           return;
		        }
		        scale = scale - 0.25;
		        displayPage();
		     });

		     $b = _b('+');
		     
		     $b.on('click', function() {
		        scale = scale + 0.25;
		        displayPage();
		     });
		
		     this.getPage = function() {
		    	 return curPage;
		     };
		     
		     this.getPageNumber = function() {
		    	 return pageNumber;
		     };
		     
		     this.scale = function(s) {
		    	 if(typeof(s) != "undefined") {
		    		 scale = s;
		    	 }
		    	 return s;
		     };
		     
		     this.render = displayPage;
		     
		     return displayPage();
	}

})(jQuery);

$(function() {
  $b = $('<button class="btn btn-info btn-xs">Show/Hide graphs</button>'),
  $b.on('click', function() {
    $('.graph .figure').toggle();
  });
  $('.graph-index').before($b);
  pdf.start().then(load_graphs);
});

const width = 540;
const height = 400;

function handle_resize() {
  var $e = $(this);
  var size = {};
  var state = $e.data('state');
  var type = $e.data('type');
  if(state) {
    size = $e.data('org');
  } else {
    if(type == "pdf") {
      var $c = $e.find('canvas');
      size = {'w': $c.attr('width'), 'h': $c.attr('height')};
    } else {
      var $c = $e.find('img');
      size = {'w': $c.prop('naturalWidth'), 'h': $c.prop('naturalHeight') };
    }
  }
  if(size) {
    $e.width(size.w);
    $e.height(size.h);
    $e.css({'max-width': size.w, 'max-height': size.h});
    state = !state;
    $e.data('state', state);    
  }
}

function load_graphs() {
  $('.graph a[href]').each(function() {
    var $a = $(this);
    var href = $a.attr('href');
    var $g = $a.parents('.graph');
    $d = $g.find('figure');
    var found = false;
    const  thumb = {'w': width+'px', 'h': height+'px'};
    
    if(href.endsWith('.pdf')) {
     found = true;
      $d.data('type','pdf');
      $d.data('org', thumb);
      $d.on('click', handle_resize);
      pdf.load($d[0], href).then(function() {
        var $c = $d.find('canvas');
        var s = {'w': $c.attr('width'), 'h': $c.attr('height')};
        $d.data("open", s);
      });
    }
    if(href.endsWith('.png') || href.endsWith('.svg')) {
      found = true;
      $d.data('type','img');
      $d.data('org', thumb);
      $d.append('<img src="'+href+'" />');
      $d.on('click', handle_resize);
    }
    if(found) {
      $d.addClass('b-1 border-light float-left figure');
      var css = {'width': width+'px','height': height+'px','max-width':width+'px','max-height':height+'px', 'overflow': 'auto'};
      $d.css(css);
    }
  });
}