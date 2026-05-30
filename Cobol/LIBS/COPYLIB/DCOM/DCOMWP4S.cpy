      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0P4: DB2PRD.TPROT_SOLTC_CANAL                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4P4S                                       *
      *    -> FETCH   - DCOM4P4F                                       *
      *    -> INSERT  - DCOM4P4I                                       *
      *    -> UPDATE  - DCOM4P4U                                       *
      *    -> DELETE  - DCOM4P4D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWP4S-HEADER.
          10 DCOMWP4S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWP4S'.
          10 DCOMWP4S-TAM-LAYOUT       PIC  9(005) VALUE 00600.
      *
          10 DCOMWP4S-BLOCO-SAIDA.
             15 DCOMWP4S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWP4S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWP4S-OCORRENCIA                OCCURS 050  TIMES.
                20 DCOMWP4S-CTPO-CANAL             PIC S9(003) COMP-3.
                20 DCOMWP4S-CCANAL-DESC            PIC  X(008).
             15 DCOMWP4S-FILLER                    PIC  X(074).
      *                                                                *
      *================================================================*
