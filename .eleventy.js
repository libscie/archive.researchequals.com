module.exports = function(eleventyConfig) {
    // Output directory: _site
    // Copy any .jpg file to `_site`, via Glob pattern
    // Keeps the same directory structure.
    eleventyConfig.addPassthroughCopy("./modules/tkj9-d4kf/Anti_Oedipe_prelude_1_2_2022.docx");
  };