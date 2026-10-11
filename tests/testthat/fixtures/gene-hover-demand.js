const assert=require('assert'),fs=require('fs'),path=require('path');
const source=fs.readFileSync(process.argv[2],'utf8');
var window={};eval(fs.readFileSync(path.join(path.dirname(process.argv[2]),'cell_views_state.js'),'utf8'));
var CBViewState=window.CBViewState, singleActive='expression_projection', linkedHoverOn=false;
var view, sent, frames, hits, metadata, point, tip={style:{}}, pinnedTip={panel:null};
var spaceById={s:{_hoverEnabled:false}};
const panel={spaceId:'s',tipId:'tip',hoverPointer:{x:10,y:10},canvas:{getBoundingClientRect:()=>({left:0,top:0})}};
function $(id){return tip;} function singleView(){return view;}
function interactionIsStatic(){return false;} function nearest(){hits++;return point;}
function singleHoverEnabledAt(s){return s._hoverEnabled;}
function setHoverCell(){} function scheduleSingleMetadata(){metadata++;}
function hoverHtml(){return 'hover';} function placeTip(){}
function requestAnimationFrame(f){frames.push(f);}
var Shiny={setInputValue:(name,value)=>sent.push({name,value})};
function extract(start,end){return source.slice(source.indexOf(start),source.indexOf(end));}
eval(extract('  function requestSingleAux()', '  function cloneBaseState('));
eval(extract('  function refreshPanelHover(', '  function wireHover('));
function flush(){while(frames.length)frames.shift()();}
function reset(){view={data:{wire_token:7},dataset_context:{generation:1}};sent=[];frames=[];hits=0;metadata=0;point=0;singleActive='expression_projection';spaceById.s._hoverEnabled=false;}
reset();refreshPanelHover(panel);refreshPanelHover(panel);flush();
assert.equal(sent.length,1);assert.equal(sent[0].value.wire_token,7);assert.equal(metadata,0);
view._auxToken=7;spaceById.s._hoverEnabled=true;refreshPanelHover(panel);flush();
assert.equal(sent.length,1);assert.equal(metadata,1);assert.equal(tip.innerHTML,'hover');
spaceById.s._hoverEnabled=false;hits=0;refreshPanelHover(panel);assert.equal(hits,0);
reset();point=-1;refreshPanelHover(panel);flush();assert.equal(sent.length,0);
reset();singleActive='overview_projection';refreshPanelHover(panel);flush();assert.equal(hits,0);assert.equal(sent.length,0);
reset();refreshPanelHover(panel);view={data:{wire_token:8},dataset_context:{generation:2}};flush();assert.equal(sent.length,0);
reset();singleActive='trajectory_projection';refreshPanelHover(panel);refreshPanelHover(panel);flush();
assert.equal(sent.length,1);assert.equal(sent[0].value.id,'trajectory_projection');
view._auxToken=7;spaceById.s._hoverEnabled=true;refreshPanelHover(panel);flush();
assert.equal(sent.length,1);assert.equal(metadata,1);assert.equal(tip.innerHTML,'hover');
reset();singleActive='trajectory_projection';point=-1;refreshPanelHover(panel);flush();assert.equal(sent.length,0);
reset();singleActive='trajectory_projection';refreshPanelHover(panel);singleActive='expression_projection';flush();assert.equal(sent.length,0);
console.log('PASS pending Gene IDs, actual-point demand, deduplication, hover settings and stale frame cancellation');
