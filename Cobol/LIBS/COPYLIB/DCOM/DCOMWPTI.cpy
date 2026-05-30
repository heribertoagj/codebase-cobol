      ****************************************************************
      * SISTEMA DCOM - DESCONTROS                                    *
      * INCLUDE      : DCOMWPTI                   LRECL=1054         *
      * DATA CRIACAO : 02/01/2023                                    *
      * RESPONSAVEL  : FOURSYS                                       *
      * DESCRICAO    : AREA DE COMUNICACAO ENTRE PROGRAMA CICS E IMS *
      ****************************************************************
          05 DCOMWPTI-REGISTRO.
             10 DCOMWPTI-ENTRADA.
                15  DCOMWPTI-TRANSACAO      PIC X(008)    VALUE SPACES.
                15  DCOMWPTI-TRANS          PIC X(008)    VALUE SPACES.
                15  DCOMWPTI-NUM-TRANS      PIC 9(003)    VALUE ZEROS.
                15  DCOMWPTI-CPRODT-BDSCO   PIC 9(003)    VALUE ZEROS.
                15  DCOMWPTI-CSPROD         PIC 9(003)    VALUE ZEROS.
                15  DCOMWPTI-DMOVTO-DCOMP   PIC X(010)    VALUE SPACES.
                15  DCOMWPTI-AREA-ENTRADA   PIC X(500)   VALUE SPACES.
             10 DCOMWPTI-SAIDA.
                15  DCOMWPTI-VUTLZ-DCOM     PIC 9(013)V99 VALUE ZEROS.
                15  DCOMWPTI-RESTR          PIC X(001)    VALUE SPACES.
                15  DCOMWPTI-AREA-SAIDA     PIC X(20000)   VALUE SPACES.
             10 DCOMWPTI-RETORNO.
                15 DCOMWPTI-COD-RETOR       PIC 9(002)    VALUE ZEROS.
                15 DCOMWPTI-COD-ERRO        PIC X(004)    VALUE SPACES.
                15 DCOMWPTI-COD-MENSAGEM    PIC X(008)    VALUE SPACES.

      *****************************************************************
      * DCOMWPTI-NUM-TRANS = 001 - EMPF9044
      * DCOMWPTI-NUM-TRANS = 002 - EMPF9046
      * DCOMWPTI-NUM-TRANS = 003 - DCOM3201
      * DCOMWPTI-NUM-TRANS = 004 - DCOM3502
      * DCOMWPTI-NUM-TRANS = 005 - COSA1010
      * DCOMWPTI-NUM-TRANS = 006 - COSA1021
      * DCOMWPTI-NUM-TRANS = 007 - COSA1030
      * DCOMWPTI-NUM-TRANS = 008 - MIDD9000
      *****************************************************************

