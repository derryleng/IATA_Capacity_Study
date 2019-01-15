function toggleSidebar() {
    var x = document.getElementsByClassName("sidebar");
    if (x.style.display === "none") {
      x.style.display = "block";
    } else {
      x.style.display = "none";
    }
  }