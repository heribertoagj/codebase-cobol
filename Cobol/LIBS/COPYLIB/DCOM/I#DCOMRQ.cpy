      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRQ                                      *
      * TAMANHO : 0060 BYTES                                           *
      * ARQUIVO : PROVISAO INADIMPLENCIA SEGMENTACAO                   *
      *           INCLUDE DCOMB0D3 - DB2PRD.TPROVS_INADP_SEGMT         *
      ******************************************************************
       01  RQ-REGISTRO.
           05 RQ-DPOSIC-CONSL-DESC    PIC X(10).
           05 RQ-CINDCD-TPO-CONSL     PIC S9(1)V USAGE COMP-3.
           05 RQ-CBCO                 PIC S9(3)V USAGE COMP-3.
           05 RQ-CSGMTO-CLI           PIC S9(3)V USAGE COMP-3.
           05 RQ-CPRODT               PIC S9(3)V USAGE COMP-3.
           05 RQ-CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
           05 RQ-CINDCD-PER-CONSL     PIC X(1).
           05 RQ-CFAIXA-PRZ-CONSL     PIC S9(3)V USAGE COMP-3.
           05 RQ-VPRINC-PCELA-CONSL   PIC S9(15)V9(2) USAGE COMP-3.
           05 RQ-VENCAR-PCELA-CONSL   PIC S9(15)V9(2) USAGE COMP-3.
           05 RQ-VTOT-POSIC-CONSL     PIC S9(15)V9(2) USAGE COMP-3.
           05 RQ-QOPER-POSIC-CONSL    PIC S9(15)V USAGE COMP-3.
           05 RQ-PFAIXA-POSIC-CONSL   PIC S9(3)V9(2) USAGE COMP-3.
