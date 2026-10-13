##----------------------------------------------------------------------------##
## Tab: About.
##----------------------------------------------------------------------------##

##
output[["about"]] <- renderText({
  ## The launcher or exporter records the version in app configuration while
  ## CerebroNexus is available. Standalone bundles therefore render this page
  ## without querying (or requiring) the package at runtime.
  version <- Cerebro.options[["cerebro_version"]]
  if (
    is.null(version) ||
      !length(version) ||
      is.na(version[[1]]) ||
      !nzchar(as.character(version[[1]]))
  ) {
    version <- "unknown"
  } else {
    version <- as.character(version[[1]])
  }
  paste0(
    '<b>Version of CerebroNexus</b><br>
    v',
    version,
    '<br>
    <br>
    <b>Authors</b><br>
    Roman Hillje<br>
    Michael Heming<br>
    Xuesong Wang<br>
    <br>
    CerebroNexus builds on the original Cerebro / cerebroApp by Roman Hillje.
    Michael Heming and Xuesong Wang continue its development as CerebroNexus,
    adding functionality beyond the original publication.<br>
    <br>
    <b>Links</b><br>
    <ul>
      <li><a href=https://github.com/mihem/CerebroNexus title="CerebroNexus on GitHub" target="_blank"><b>CerebroNexus on GitHub</b></a></li>
      <li><a href=https://github.com/romanhaa/Cerebro title="Discontinued Cerebro repository on GitHub (Roman Hillje)" target="_blank"><b>Discontinued Cerebro repository on GitHub (Roman Hillje)</b></a></li>
    </ul>
    <br>
    <b>Citation</b><br>
    If you used CerebroNexus for your research, please cite both the software
    used and the original Cerebro publication:
    <ol>
      <li>Michael Heming, Roman Hillje, Xuesong Wang.
      <i>CerebroNexus</i> [software]. Version ', htmltools::htmlEscape(version), '.
      <a href="https://github.com/mihem/CerebroNexus" target="_blank" rel="noopener noreferrer">https://github.com/mihem/CerebroNexus</a>.
      Record the version or commit you used; if using a fork, also include its repository URL and commit.</li>
      <li>Roman Hillje, Pier Giuseppe Pelicci, Lucilla Luzi.
      <i>Cerebro: Interactive visualization of scRNA-seq data</i>.
      Bioinformatics, btz877.
      <a href="https://doi.org/10.1093/bioinformatics/btz877" title="DOI" target="_blank" rel="noopener noreferrer">https://doi.org/10.1093/bioinformatics/btz877</a>.</li>
    </ol>
    <br>
    <b>License</b><br>
    CerebroNexus is distributed under the terms of the <a href=https://github.com/mihem/CerebroNexus/blob/master/LICENSE.md title="MIT license" target="_blank">MIT license.</a><br>
    <br>
    <b>Credit where credit is due</b><br>
    <ul>
      <li>The default plot palettes are a custom low-saturation set; several colours draw on <a href="https://flatuicolors.com/" title="Flat UI Colors 2" target="_blank">Flat UI Colors 2</a>.</li>
    </ul>
    <br>'
  )
})
