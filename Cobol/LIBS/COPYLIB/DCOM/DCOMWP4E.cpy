      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0P4: DB2PRD.TPARM_CANAL_DESC                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4P4S                                       *
      *    -> FETCH   - DCOM4P4F                                       *
      *    -> INSERT  - DCOM4P4I                                       *
      *    -> UPDATE  - DCOM4P4U                                       *
      *    -> DELETE  - DCOM4P4D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWP4E-HEADER.
          10 DCOMWP4E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWP4E'.
          10 DCOMWP4E-TAM-LAYOUT       PIC  9(005) VALUE 00200.
      *
       05 DCOMWP4E-BLOCO-ENTRADA.
          10 DCOMWP4E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWP4E-ENTRADA.
             15 DCOMWP4E-CTPO-CANAL                PIC S9(003) COMP-3.
             15 DCOMWP4E-CCANAL-DESC               PIC  X(008).
             15 DCOMWP4E-FILLER                    PIC  X(020).
      *
          10 DCOMWP4E-BLOCO-VARIAVEL.
             15 DCOMWP4E-V-FILLER                  PIC  X(100).
      *
          10 DCOMWP4E-BLOCO-PAGINACAO.
             15 DCOMWP4E-MAX-OCORR                 PIC  9(003).
             15 DCOMWP4E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWP4E-P-INICIAL              VALUE 'I'.
                88 DCOMWP4E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWP4E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWP4E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWP4E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWP4E-BLOCO-RESTART.
             15 DCOMWP4E-CHAVE-INI.
                20 DCOMWP4E-I-CTPO-CANAL           PIC S9(003) COMP-3.
                20 DCOMWP4E-I-FILLER               PIC  X(015).
             15 DCOMWP4E-CHAVE-FIM.
                20 DCOMWP4E-F-CTPO-CANAL           PIC S9(003) COMP-3.
                20 DCOMWP4E-F-FILLER               PIC  X(015).
          10 DCOMWP4E-FILLER                       PIC  X(009).
