document.addEventListener("DOMContentLoaded", function () {
  const sidebar = document.querySelector("nav#toc"); // bs4_book sidebar

  if (sidebar) {
    const customLink = document.createElement("div");
    customLink.innerHTML = `
      <div class="custom-sidebar-link" style="margin: 1em 0;">
        <a href="https://drfloreiche.github.io/" style="color: #e1e2ea; font-weight: 500; text-decoration: none;">
          Back to Flo's Home
        </a>
      </div>
    `;

    const searchbox = sidebar.querySelector(".book-search");

    if (searchbox) {
      // insert after searchbox
      searchbox.parentNode.insertBefore(customLink, searchbox.nextSibling);
    } else {
      sidebar.appendChild(customLink); // fallback: append at end
    }
  }
});
