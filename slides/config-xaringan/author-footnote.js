//----------------------------------------------------------------------
//                                           Prof. Dr. Walmes M. Zeviani
//                               leg.ufpr.br/~walmes · github.com/walmes
//                                       walmes@ufpr.br · @walmeszeviani
//                     Laboratory of Statistics and Geoinformation (LEG)
//               Department of Statistics · Federal University of Paraná
//                                      2021-mar-16 · Curitiba/PR/Brazil
//----------------------------------------------------------------------

// Este script coloca o rodapé no canto inferior esquerdo. É necessário
// definir no CSS a classe `author-footnote` com o conteúdo a ser
// exibido.

document
  .querySelectorAll(
    '.remark-slide-content' +
    ':not(.title-slide)'
  )
  .forEach(el => {
    el.innerHTML += '<div class="author-footnote"></div>';
  });

//----------------------------------------------------------------------
