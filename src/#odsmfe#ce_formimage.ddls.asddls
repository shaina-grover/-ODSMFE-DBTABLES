@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'Custom entity for formimage'

define custom entity /ODSMFE/CE_FORMIMAGE
{


   key  FormID            : /odsmfe/de_formid;

   key  Version           : /odsmfe/de_version;
   
   @EndUserText.label: 'Question'
   key  Question         : abap.char(30);
      
        @EndUserText.label: 'File Name'
        FileName        :abap.char(100);
        
        @EndUserText.label: 'Media Type'
        MediaType       :abap.char(30);
     
        @EndUserText.label: 'BASE64'
        Base64           : abap.string( 0 );
          
      
    }
