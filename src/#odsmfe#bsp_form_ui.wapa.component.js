sap.ui.define(["sap/ui/core/UIComponent","sap/ui/Device","odsmfe/FormUI/model/models"],function(e,t,i){"use strict";return e.extend("odsmfe.FormUI.Component",{metadata:{manifest:"json"},init:function(){e.prototype.init.apply(this,arguments);this.getRoute+
r().initialize();this.setModel(i.createDeviceModel(),"device")}})});                                                                                                                                                                                           