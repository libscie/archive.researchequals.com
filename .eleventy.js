module.exports = function(eleventyConfig) {
    // Output directory: _site
    // Copy any .jpg file to `_site`, via Glob pattern
    // Keeps the same directory structure.
    eleventyConfig.addPassthroughCopy("./modules/**");
    eleventyConfig.addPassthroughCopy("./collections/**");
    eleventyConfig.addPassthroughCopy("./_includes/*");    
  };