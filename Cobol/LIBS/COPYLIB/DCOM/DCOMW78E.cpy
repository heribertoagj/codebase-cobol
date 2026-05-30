      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB078: DB2PRD.TTPO_AGPTO_ROTRO                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM478S                                       *
      *    -> FETCH   - DCOM478F                                       *
      *    -> INSERT  - DCOM478I                                       *
      *    -> UPDATE  - DCOM478U                                       *
      *    -> DELETE  - DCOM478D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW78E-HEADER.
          10 DCOMW78E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW78E'.
          10 DCOMW78E-TAM-LAYOUT       PIC  9(005) VALUE 00350.
      *
       05 DCOMW78E-BLOCO-ENTRADA.
          10 DCOMW78E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW78E-ENTRADA.
             15 DCOMW78E-CTPO-AGPTO-ROTRO          PIC S9(003) COMP-3.
             15 DCOMW78E-ITPO-AGPTO-ROTRO          PIC  X(025).
             15 DCOMW78E-IRSUMO-AGPTO-ROTRO        PIC  X(015).
             15 DCOMW78E-CSGL-AGPTO-ROTRO          PIC  X(006).
             15 DCOMW78E-HULT-ATULZ                PIC  X(026).
             15 DCOMW78E-CTRANS-PROG-ATULZ         PIC  X(008).
             15 DCOMW78E-FILLER                    PIC  X(040).
      *
          10 DCOMW78E-BLOCO-VARIAVEL.
             15 DCOMW78E-V-FILLER                  PIC  X(100).
      *
          10 DCOMW78E-BLOCO-PAGINACAO.
             15 DCOMW78E-MAX-OCORR                 PIC  9(003).
             15 DCOMW78E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW78E-P-INICIAL              VALUE 'I'.
                88 DCOMW78E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW78E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW78E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW78E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW78E-BLOCO-RESTART.
             15 DCOMW78E-CHAVE-INI.
                20 DCOMW78E-I-CTPO-AGPTO-ROTRO     PIC S9(003) COMP-3.
                20 DCOMW78E-I-FILLER               PIC  X(010).
             15 DCOMW78E-CHAVE-FIM.
                20 DCOMW78E-F-CTPO-AGPTO-ROTRO     PIC S9(003) COMP-3.
                20 DCOMW78E-F-FILLER               PIC  X(010).
      *
          10 DCOMW78E-FILLER                       PIC  X(077).
      *
