      ******************************************************************
      * SISTEMA : DCOM -  ESTATISTICAS NET EMPRESA -          I#DCOMU3 *
      * TAMANHO : 0150 BYTES   BAIXAS SUMARIZADO                       *
      * ARQUIVO : BAIXA SUMARIZADA POR BCO/AGEN/CTA/PROD/SUBPROD       *
      * INCLUDE DA TABELA DCOMB094 - DB2PRD.TBAIXA_PCELA_DESC          *
      ******************************************************************
 01    01 U3-REGISTRO.
 01       05 U3-CHAVE.
 01          10 U3-CCORR.
 01 02          15 U3-CBCO           PIC  S9(003)V     COMP-3.
 03 03          15 U3-CAG-BCRIA      PIC  S9(005)V     COMP-3.
 06 07          15 U3-CCTA-BCRIA-CLI PIC  S9(013)V     COMP-3.
 13 02       10 U3-CPRODT            PIC  S9(003)V     COMP-3.
 15 02       10 U3-CSPROD-DESC-COML  PIC  S9(003)V     COMP-3.
 17 08       10 U3-DEFETV-BAIXA-PCELA PIC   9(008).
 17          10 FILLER  REDEFINES    U3-DEFETV-BAIXA-PCELA.
 17 04          15 U3-DANOBAIXA      PIC   9(004).
 21 02          15 U3-DMESDBAIXA     PIC   9(002).
 23 02          15 U3-DDIADBAIXA     PIC   9(002).
 25 09    05 U3-VPOSIC-BAIXA-DIA
                                     PIC S9(15)V9(2) USAGE COMP-3.
 34 09    05 U3-VPOSIC-BAIXA-MES
                                     PIC S9(15)V9(2) USAGE COMP-3.
 43 09    05 U3-QTITLO-BAIXA-MES
                                     PIC S9(17)      USAGE COMP-3.
 52 09    05 U3-VPOSIC-PGTO-BDSCO-MES
                                     PIC S9(15)V9(2) USAGE COMP-3.
 61 09    05 U3-QTITLO-PGTO-BDSCO-MES
                                     PIC S9(17)      USAGE COMP-3.
 70 09    05 U3-VPOSIC-PGTO-OUTRO-BCO-MES
                                     PIC S9(15)V9(2) USAGE COMP-3.
 79 09    05 U3-QTITLO-PGTO-OUTRO-BCO-MES
                                     PIC S9(17)      USAGE COMP-3.
 88 09    05 U3-VPOSIC-PGTO-DEB-CTA-MES
                                     PIC S9(15)V9(2) USAGE COMP-3.
 97 09    05 U3-QTITLO-PGTO-DEB-CTA-MES
                                     PIC S9(17)      USAGE COMP-3.
106 09    05 U3-VPOSIC-VENCD-DIA
                                     PIC S9(15)V9(2) USAGE COMP-3.
115 09    05 U3-QTITLO-VENCD-DIA
                                     PIC S9(17)      USAGE COMP-3.
124 06    05 FILLER                  PIC  X(27).
