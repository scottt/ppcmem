
function search_for_file(model,litmus,folder,index){
	if(index == (folder.length - 1)){	
		$('#litmus_test').val('');
		$('#warnings').html("Test not found");
		$('#warnings').css('display','block');
	}
	else{
		//Do a recursive search
		$.ajax({
			url: "src-".concat(model).concat('/').concat(folder[index]),
			type: "GET",
			asynch:false,
			dataType: "text",
			cache: true,
			success: function(res){
				if(res.indexOf(litmus.concat('.litmus')) >=  0){
					//MATCH FOUND
					var file = folder[index].substring(0,folder[index].length-4);
					fetch("src-".concat(model).concat("/").concat(file).concat("/").concat(litmus));
				}
				else{
					search_for_file(model,litmus,folder,index+1);
				}
			},
			error: function(){
				$('#litmus_test').val('');
			}
		});
	}
}


function fetch(x){
	if(x == undefined){
		var url = document.location.href;
		var test = url.substring((url.indexOf('#')+1),url.length);
		var model = test.substring(0,3).toLowerCase();
		var litmus = test.substring(4);
		/*****FETCH THE LIST OF ALL THE @all files*****/
		$.ajax({
			url: "src-".concat(model).concat('/').concat('@all'),
			type: "GET",
			asynch:false,
			dataType: "text",
			cache: true,
			success: function(res){
				/*$('#litmus_test').val(res);
				$('#litmus_test').trigger('resize');*/
				search_for_file(model,litmus,res.split('\n'),0);
				},
			error: function(){
				$('#litmus_test').val('');
				}
		});
	}
	else{
		test = x.concat(".litmus"); 
		$.ajax({
			url: test,
			type: "GET",
			dataType: "text",
			cache: true,
			success: function(res){
					$('#litmus_test').val(res);
					$('#litmus_test').trigger('resize');
			},
			error: function(){
				$('#litmus_test').val('');
				$('#warnings').css('display','block');
				document.getElementById('warnings').innerHTML = "Test not found";
			}
		});
	}
}
//fetch();
function setCookie(name,value) {
	document.cookie = name+"="+value;
}

function getCookie(name) {
	var temp = name + "=";
	var cookies = document.cookie.split(';');
	for(var i=0;i < cookies.length;i++) {
		var cookie = cookies[i];
		cookie = $.trim(cookie);
		if (cookie.indexOf(temp) == 0) return cookie.substring(temp.length,cookie.length);
	}
	return null;
}

function deleteCookie(name) {
    setCookie(name,"",-1);
}

var selected_model = '';

function fetch_filenames(folder){
		$.ajax({
			url: "src-".concat(selected_model).concat("/").concat(folder).concat("/@all"),
			type: "GET",
			dataType: "text",
			asynch:false,
			cache: true,
			success: function(res){
				var files = res.split("\n");
				var tests = [];
				//files = files.slice(2,(files.length - 1));
				var patt = /[#]/;
				for(i = 0,j = 0; i<files.length-1; i++){
					if(patt.test(files[i]) == false){
						tests[j] = files[i].replace(".litmus","");j++;
					}
				}
				tests.sort();
				var left = [];
				var right = [];
				for(i = 0;i<Math.floor(tests.length/2); i++){
					left[i] = "<div class='test_l'>".concat(tests[i]).concat("</div>");
					}
				for(i = Math.floor(tests.length/2),j = 0;i<tests.length; i++){
					right[j] = "<div class='test_r'>".concat(tests[i]).concat("</div>");
					j++;
					}
				var path = '#_'.concat(selected_model).concat('_> .contents >div');
				$(path).parent().data('folder',folder);
				$(path)[0].innerHTML = (left.join(""));
				$(path)[1].innerHTML = (right.join(""));
				$(path)[2].innerHTML = ("");
			},
			error: function(){
				var path = '#_'.concat(selected_model).concat('_> .contents >div');
				$(path)[0].innerHTML = ("");
				$(path)[1].innerHTML = ("");
				$(path)[2].innerHTML = ("Network Error: Unable to fetch tests</b>");
			}
		});
}

$('#help').live('click',function(){
	window.open('help.html');
	return false;	
});

$('#test_select').live('click',function (){
	$('#main').css('display','none');
	$('#left').addClass('disabled');
	$('#right').addClass('disabled');
	$('.scroll').addClass('disabled');
	$('#click').hide();
	if(selected_model == 'arm'){
		$('#_arm_').css('display','block');
		$('#_arm_> .category_wrapper > .category > ul >li').eq(0).mouseover();
		//$('#_arm_> .contents').css('min-height',parseInt($('#_arm_> .category_wrapper > .category').css('height')) + 30);
		$('#_arm_> .contents').css('min-height', '450px');
	}
	else{
		$('#_ppc_').css('display','block');
		$('#_ppc_> .category_wrapper >.category >ul >li').eq(0).mouseover();
	//	$('#_ppc_>.contents ').css('min-height',parseInt($('#_ppc_> .category_wrapper > .category').css('height')) + 30);
		$('#_ppc_> .contents').css('min-height', '450px');
	}
	$('#warnings').html("");
	$('#main_container').css('margin-left',$('#main_container').offset().left);
	
});

$('.test_l, .test_r').live('click',function(){
	var folder = $(this).parent().parent().data('folder'); 
	fetch("src-".concat(selected_model).concat("/").concat(folder).concat("/").concat($(this).text()));	
	$('.browse').css('display','none');
	$('#main').css('display','block');
	$('#left').removeClass('disabled');
	$('#right').removeClass('disabled');
	$('.scroll').removeClass('disabled');
	$('#click').show();
	$('#main_container').css('margin','auto');
});

$('.trans').live('click',function(){
	$('#transitions input')[parseInt($(this).attr('id'))].click();
});

$('.close').live('click',function(){
	$('.browse').css('display','none');
	$('#main').css('display','block');
	$('#left').removeClass('disabled');
	$('#right').removeClass('disabled');
	$('.scroll').removeClass('disabled');
	$('#click').show();
	$('#main_container').css('margin','auto');
});	
$('#left:not(.disabled)').live('click',function(){
	if($('#ppc_title').css('top') == '0px'){
		$('#ppc_title').animate({'top':'-70'},800,'swing');
		$('#arm_title').animate({'top':'-70'},800,'swing');
		document.title='ARM Memory Model';
		selected_model = 'arm';
		$('#test_select').val("Select ARM Test");
		$('#click').html('Change to POWER Model');
		$('#left').hide();
		$('#right').show();
	}
});
$('#click').live('click',function(){
	$('.scroll').click();
	});
$('#right:not(.disabled)').live('click',function(){
	if($('#ppc_title').css('top') == '-70px'){
		$('#ppc_title').animate({'top':'0'},800,'swing');
		$('#arm_title').animate({'top':'0'},800,'swing');
		document.title='POWER Memory Model';
		selected_model = 'ppc';
		$('#test_select').val("Select POWER Test");
		$('#click').html('Change to ARM Model');
		$('#right').hide();
		$('#left').show();
	}
});
$('.scroll:not(.disabled)').live('click',function(){
	if(selected_model == 'ppc')
		$('#left').click();
	else
		$('#right').click();
});
$('#litmus_test').live('resize',function(){
	x = $('#litmus_test').val().split('\n');
	if(x == "") return ;
	var max = x[0].length ;
	for(i = 1;i<x.length;i++)
    	if(max < x[i].length)max = x[i].length;
	$('#litmus_test').attr('cols',max);
	$('#litmus_test').attr('rows',x.length);
});
function setModel(model){
	if(model == 'arm'){
		document.title = "ARM Memory Model";
		selected_model = 'arm';
		$('#ppc_title').css('top','-70px');
		$('#arm_title').css('top','-70px');
		$('#test_select').val("Select ARM Test");
		$('#click').html('Change to POWER Model');
		$('#right').show();
		$('#left').hide();
	}
	else if(model == 'ppc'){
		document.title = "POWER Memory Model";
		selected_model = 'ppc';
		$('#ppc_title').css('top','0px');
		$('#arm_title').css('top','0px');
		$('#test_select').val("Select POWER Test");
		$('#click').html('Change to ARM Model');
		$('#left').show();
		$('#right').hide();
	}
}

$('#interact').live('click',function(){
	if($('#litmus_test').css('display') == 'none'){
		var model = $('#litmus_test').val().substring(0,3).toLowerCase(); 
		if(model == 'arm')setModel('arm');
		else if(model == 'ppc')setModel('ppc');
		$('#left').addClass('disabled');
		$('#right').addClass('disabled');
		$('.scroll').addClass('disabled');
		$('#right').hide();
		$('#left').hide();
		$('#click').hide();
	}
});

function makeScrollable(wrapper, scrollable){
  var wrapper = $(wrapper), scrollable = $(scrollable);

      setTimeout(function(){

        wrapper.css({overflow: "hidden"});
          enable(); 
		  }, 1000); 

  function enable(){
	  var inactiveMargin = 50;         
	  var wrapperWidth = wrapper.width();
	  var wrapperHeight = wrapper.height();
      
	  wrapper.mousemove(function(e){
			  var scrollableHeight = scrollable.outerHeight() + 2*inactiveMargin;
			  var wrapperOffset = wrapper.offset();
			  var top = (e.pageY -  wrapperOffset.top) * (scrollableHeight - wrapperHeight) / wrapperHeight  - inactiveMargin;

			  if (top < 0){
				  top = 0;
			  }
			  wrapper.scrollTop(top);
			  });       
  }
}

$(document).ready(function () {

	function ext_onload(){
		var url = document.location.href;
		var index = url.indexOf('#');
		if ( index != -1 && url.substring((index + 1), url.length).length > 4){
			fetch();
		}
		else if(index == -1){
		//	$('#litmus_test').trigger('resize');
			if($('#litmus_test').val() == "" && getCookie('litmus') != null){
				$('#litmus_test').val(unescape(getCookie('litmus')));
				$('#litmus_test').trigger('resize');
			}
		}
		if(selected_model == null || selected_model == 'ppc'){
			$('#test_select').val("Select POWER Test");
		}
		else{
			$('#test_select').val("Select ARM Test");
		}
	}

	window.addEventListener("load",ext_onload, false);

	$('#reset').live('click',function(){
		if($('#litmus_test').css('display') == 'none'){
			setCookie("litmus",escape($('#litmus_test').val()));
			setCookie("model",escape(selected_model));
		}
		/* TODO:  reset without reloading.*/
		var url = document.location.href;
		document.location.href = url.substring(0,url.indexOf('#'));
	});

	var Default = false;
	var url = document.location.href;
	if (url.indexOf('#') != -1){
		var test = url.substring((url.indexOf('#')+1),url.length);
		var model = test.substring(0,3).toLowerCase();
		if(model == 'arm'){
		/*	document.title = 'ARM Memory Model';
			selected_model = model;
			$('#ppc_title').css('left','-480px');
			$('#arm_title').css('left','-480px');
			//$('#left').click();*/
			setModel('arm');
		}
		else if(model == 'ppc'){
		/*	document.title = 'The Power PC Memory Model';
			selected_model = model;
			$('#ppc_title').css('left','0px');
			$('#arm_title').css('left','0px');
			//$('#right').click();*/
			setModel('ppc');
		}
		else{
			Default = true;
		}
	}
	else{
		Default = true;
	}
	if(Default){
		selected_model = getCookie('model');
		if(selected_model == null || selected_model == 'ppc'){
			setModel('ppc');
		}
		else{
			setModel('arm');
		}
	}

	var $ppcitems = $('#_ppc_> .category_wrapper >.category > ul > li');
	$ppcitems.mouseover(function() {
		$ppcitems.removeClass('selected');
		$(this).addClass('selected');
		$('#_ppc_> .contents >h4').html($(this).text());
		var folder = $.trim($(this).text().split(':')[0]);
		$(this).data('folder',folder);
		fetch_filenames($.trim($(this).text().split(':')[0]))
	});

	var $armitems = $('#_arm_> .category_wrapper>.category > ul > li');
	$armitems.mouseover(function() {
		$armitems.removeClass('selected');
		$(this).addClass('selected');
		$('#_arm_> .contents >h4').html($(this).text());
		fetch_filenames($.trim($(this).text().split(':')[0]))
	});
	makeScrollable("#_arm_ > .category_wrapper","#_arm_ >.category_wrapper > .category");
	makeScrollable("#_ppc_ > .category_wrapper","#_ppc_ >.category_wrapper > .category");
	
	$('#select_options').live('click', function(e) {
		e.preventDefault();
		
		var id = "#options";
	
		var maskHeight = $(document).height();
		var maskWidth = $(window).width();
	
		$('#mask').css({'width':maskWidth,'height':maskHeight});
		
//		$('#mask').fadeIn(1000);	
		$('#mask').fadeTo("fast",0.8);	
	
		var winH = $(window).height();
		var winW = $(window).width();
              
		$(id).css('top',  winH/2-$(id).height()/2);
		$(id).css('left', winW/2-$(id).width()/2);
	
		$(id).fadeIn("fast"); 
	
	});
	
	$('.close_options').live('click',function (e) {
		e.preventDefault();
		
		$('#mask').hide();
		$('.window').hide();
	});		
	
	$('#mask').click(function () {
		$(this).hide();
		$('.window').hide();
	});

});


