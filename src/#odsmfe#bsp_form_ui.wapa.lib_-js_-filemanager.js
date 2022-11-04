import $ from"jquery";import{getFilename,dataUriToBlobSync}from"./utils";const fileManager={};import{t}from"enketo/translator";const URL_RE=/[a-zA-Z0-9+-.]+?:\/\//;fileManager.init=(()=>{return Promise.resolve(true)});fileManager.isWaitingForPermissions=+
(()=>{return false});fileManager.getFileUrl=(e=>{return new Promise((r,a)=>{let i;if(!e){r(null)}else if(typeof e==="string"){if(URL_RE.test(e)){r(e)}else{a("no!")}}else if(typeof e==="object"){if(fileManager.isTooLarge(e)){i=new Error(t("filepicker.tool+
argeerror",{maxSize:fileManager.getMaxSizeReadable()}));a(i)}else{r(URL.createObjectURL(e))}}else{a(new Error("Unknown error occurred"))}})});fileManager.getObjectUrl=(e=>fileManager.getFileUrl(e).then(e=>{if(/https?:\/\//.test(e)){return fileManager.url+
ToBlob(e).then(URL.createObjectURL)}return e}));fileManager.urlToBlob=(e=>{const t=new XMLHttpRequest;return new Promise(r=>{t.open("GET",e);t.responseType="blob";t.onload=(()=>{r(t.response)});t.send()})});fileManager.getCurrentFiles=(()=>{const e=[];$(+
"form.or").find('input[type="file"]:not(.ignore), input[type="text"][data-drawing="true"]').each(function(){let t;let r=null;let a=null;if(this.type==="file"){r=this.files[0]}else if(this.value){a=$(this).closest(".question")[0].querySelector(".draw-widg+
et canvas");if(a&&!URL_RE.test(this.value)){r=dataUriToBlobSync(a.toDataURL());r.name=this.value}}if(r&&r.name){t=getFilename(r,this.dataset.filenamePostfix);if(this.dataset.resized&&this.dataset.resizedDataURI){r=dataUriToBlobSync(this.dataset.resizedDa+
taURI)}r=new Blob([r],{type:r.type});r.name=t;e.push(r)}});return e});fileManager.isTooLarge=(()=>{return false});fileManager.getMaxSizeReadable=(()=>{return`${5}MB`});export default fileManager;                                                            