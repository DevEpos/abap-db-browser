INTERFACE zif_dbbr_c_aggregation
  PUBLIC .
  CONSTANTS sum     TYPE zdbbr_aggr VALUE 'SUM' ##no_text.
  CONSTANTS minimum TYPE zdbbr_aggr VALUE 'MIN' ##no_text.
  CONSTANTS maximum TYPE zdbbr_aggr VALUE 'MAX' ##no_text.
  CONSTANTS average TYPE zdbbr_aggr VALUE 'AVG' ##no_text.
ENDINTERFACE.
