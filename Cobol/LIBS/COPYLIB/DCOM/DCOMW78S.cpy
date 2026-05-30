      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMW78S-HEADER.
          10 DCOMW78S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW78S'.
          10 DCOMW78S-TAM-LAYOUT       PIC  9(005) VALUE 04300.
      *
          10 DCOMW78S-BLOCO-SAIDA.
             15 DCOMW78S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW78S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW78S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW78S-CTPO-AGPTO-ROTRO       PIC S9(003) COMP-3.
                20 DCOMW78S-ITPO-AGPTO-ROTRO       PIC  X(025).
                20 DCOMW78S-IRSUMO-AGPTO-ROTRO     PIC  X(015).
                20 DCOMW78S-CSGL-AGPTO-ROTRO       PIC  X(006).
                20 DCOMW78S-HULT-ATULZ             PIC  X(026).
                20 DCOMW78S-CTRANS-PROG-ATULZ      PIC  X(008).
             15 DCOMW78S-FILLER                    PIC  X(174).
      *
