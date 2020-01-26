var sl=document.getElementById("statusline");
    if(typeof(sl) !== 'undefined') {
      sl.ontouchstart= function(e) {
         foo();
      };
    }