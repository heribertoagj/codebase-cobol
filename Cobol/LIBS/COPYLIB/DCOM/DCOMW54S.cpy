      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB054: DB2PRD.TROTRO_ELMTO_DESC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM454S                                       *
      *    -> FETCH   - DCOM454F                                       *
      *    -> INSERT  - DCOM454I                                       *
      *    -> UPDATE  - DCOM454U                                       *
      *    -> DELETE  - DCOM454D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW54S-HEADER.
          10 DCOMW54S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMW54S'.
          10 DCOMW54S-TAM-LAYOUT   PIC  9(005) VALUE 2450.
      *
          10 DCOMW54S-BLOCO-SAIDA.
             15 DCOMW54S-QTDE-TOTAL            PIC  9(010).
             15 DCOMW54S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMW54S-OCORRENCIA            OCCURS 050 TIMES.
                20 DCOMW54S-CELMTO-DESC-COML   PIC S9(003) COMP-3.
                20 DCOMW54S-CPRODT             PIC S9(003) COMP-3.
                20 DCOMW54S-CTPO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW54S-CMEIO-ENTRD-DESC   PIC S9(003) COMP-3.
                20 DCOMW54S-CTPO-AGPTO-ROTRO   PIC S9(003) COMP-3.
                20 DCOMW54S-CINDCD-OBRIG-AGPTO PIC  X(001).
                20 DCOMW54S-CTRANS-PROG-ATULZ  PIC  X(008).
                20 DCOMW54S-HULT-ATULZ         PIC  X(026).
             15 DCOMW54S-FILLER                PIC  X(174).
      *
