document.addEventListener("DOMContentLoaded", function() {
  var images = document.querySelectorAll("main img");

  images.forEach(function(img) {
    if (img.closest("a, .html-widget")) {
      return;
    }
    img.style.cursor = "zoom-in";
    img.tabIndex = 0;
    img.setAttribute("role", "button");
    img.setAttribute("aria-haspopup", "dialog");
    img.setAttribute("aria-label", figureActionLabel(img));

    img.addEventListener("click", function(event) {
      event.preventDefault();
      showImageInModal(img);
    });

    img.addEventListener("keydown", function(event) {
      if (event.key === "Enter" || event.key === " ") {
        event.preventDefault();
        showImageInModal(img);
      }
    });
  });

  function figureActionLabel(img) {
    if (img.alt) {
      return "Enlarge figure: " + img.alt;
    }
    return "Enlarge figure";
  }

  function showImageInModal(imgElement) {
    if (document.querySelector("[data-figure-modal]")) {
      return;
    }

    var previousOverflow = document.body.style.overflow;
    var modal = document.createElement("div");
    modal.setAttribute("data-figure-modal", "");
    modal.setAttribute("role", "dialog");
    modal.setAttribute("aria-modal", "true");
    modal.setAttribute(
      "aria-label",
      imgElement.alt ? "Enlarged figure: " + imgElement.alt : "Enlarged figure"
    );
    modal.style.position = "fixed";
    modal.style.inset = "0";
    modal.style.padding = "2rem min(2rem, 2vw)";
    modal.style.backgroundColor = "rgba(0,0,0,0.88)";
    modal.style.display = "flex";
    modal.style.justifyContent = "center";
    modal.style.alignItems = "center";
    modal.style.zIndex = "1080";
    modal.style.cursor = "zoom-out";

    var fullImage = document.createElement("img");
    fullImage.src = imgElement.currentSrc || imgElement.src;
    fullImage.alt = imgElement.alt;
    fullImage.style.width = "100%";
    fullImage.style.height = "100%";
    fullImage.style.objectFit = "contain";
    fullImage.style.cursor = "zoom-out";
    modal.appendChild(fullImage);

    var closeButton = document.createElement("button");
    closeButton.type = "button";
    closeButton.setAttribute("aria-label", "Close enlarged figure");
    closeButton.textContent = "\u00d7";
    closeButton.style.position = "absolute";
    closeButton.style.top = "0.5rem";
    closeButton.style.right = "0.75rem";
    closeButton.style.border = "0";
    closeButton.style.background = "transparent";
    closeButton.style.color = "white";
    closeButton.style.fontSize = "2.5rem";
    closeButton.style.lineHeight = "1";
    closeButton.style.cursor = "pointer";
    modal.appendChild(closeButton);

    function closeModal() {
      if (!modal.isConnected) {
        return;
      }
      document.removeEventListener("keydown", handleModalKeydown);
      document.body.style.overflow = previousOverflow;
      modal.remove();
      imgElement.focus();
    }

    function handleModalKeydown(event) {
      if (event.key === "Escape") {
        event.preventDefault();
        closeModal();
      } else if (event.key === "Tab") {
        event.preventDefault();
        closeButton.focus();
      }
    }

    modal.addEventListener("click", function(event) {
      if (event.target === modal || event.target === fullImage) {
        closeModal();
      }
    });
    closeButton.addEventListener("click", closeModal);
    document.addEventListener("keydown", handleModalKeydown);

    document.body.style.overflow = "hidden";
    document.body.appendChild(modal);
    closeButton.focus();
  }
});
