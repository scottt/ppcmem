function caml_sys_getenv(x){
	return "";
}

function caml_ml_output_char(m,n){
	return "";
}

function caml_ml_input_scan_line(x){
	return "";
}

/*GLOBAL EVENT HANDLER*/

window.onerror=function(msg, url, linenumber){
  $('#warnings').css('display','block');
  $('#warnings').text(msg.split(',')[3]);
  return true;
}

function caml_ml_input(){
	return "";
}
