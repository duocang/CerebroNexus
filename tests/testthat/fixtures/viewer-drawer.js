() => {
  const drawer = document.querySelector('.cerebro-settings-drawer[aria-hidden="false"]');
  const r = drawer.getBoundingClientRect();
  const visible = el => el.getClientRects().length && getComputedStyle(el).visibility !== 'hidden';
  const controls = Array.from(drawer.querySelectorAll('.form-group, .checkbox'))
    .filter(el => visible(el) && !el.parentElement.closest('.form-group, .checkbox'))
    .map(el => el.getBoundingClientRect()).filter(b => b.width > 0 && b.height > 0);
  const modal = innerWidth <= 900;
  const focusable = Array.from(drawer.querySelectorAll('button:not([disabled]), a[href], input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"])')).filter(visible);
  let wrapsFocus = true;
  if (modal && focusable.length > 1) {
    focusable.at(-1).focus();
    document.dispatchEvent(new KeyboardEvent('keydown', {key:'Tab', bubbles:true, cancelable:true}));
    wrapsFocus = document.activeElement === focusable[0];
    document.dispatchEvent(new KeyboardEvent('keydown', {key:'Tab', shiftKey:true, bubbles:true, cancelable:true}));
    wrapsFocus = wrapsFocus && document.activeElement === focusable.at(-1);
  }
  return {
    modalSemantics: drawer.getAttribute('aria-modal') === String(modal),
    backgroundInert: !modal || Array.from(document.body.children).filter(el => el !== drawer).every(el => el.inert),
    wrapsFocus,
    reducedMotion: [getComputedStyle(drawer).transitionDuration, getComputedStyle(drawer).animationDuration]
      .every(value => value.split(',').every(duration => parseFloat(duration) === 0)),
    fitsViewport: r.left >= -1 && r.right <= innerWidth+1 && r.top >= -1 && r.bottom <= innerHeight+1,
    fitsContents: drawer.scrollWidth <= drawer.clientWidth+1 && controls.every(b => b.left >= r.left-1 && b.right <= r.right+1),
    overlaps: controls.some((a, i) => controls.slice(i+1).some(b =>
      Math.min(a.right,b.right)-Math.max(a.left,b.left)>1 &&
      Math.min(a.bottom,b.bottom)-Math.max(a.top,b.top)>1)),
    controls: controls.length
  };
}
