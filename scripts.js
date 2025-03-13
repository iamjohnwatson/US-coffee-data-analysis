document.querySelectorAll('a[href^="#"]').forEach(anchor => {
    anchor.addEventListener('click', function(e) {
      e.preventDefault();
      document.querySelector(this.getAttribute('href')).scrollIntoView({ behavior: 'smooth' });
    });
  });
  window.addEventListener('resize', () => {
    document.querySelectorAll('.plot-container').forEach(container => {
      Plotly.Plots.resize(container);
    });
  });