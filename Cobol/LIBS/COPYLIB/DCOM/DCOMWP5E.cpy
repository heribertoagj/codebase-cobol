      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0P5: DB2PRD.TPROT_SOLTC_AGNDA                       *
      *----------------------------------------------------------------*
      * P5DULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4P5S                                       *
      *    -> FETCH   - DCOM4P5F                                       *
      *    -> INSERT  - DCOM4P5I                                       *
      *    -> UPDATE  - DCOM4P5U                                       *
      *    -> DELETE  - DCOM4P5D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWP5E-HEADER.
          10 DCOMWP5E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWP5E'.
          10 DCOMWP5E-TAM-LAYOUT       PIC  9(005) VALUE 01073.
      *
       05 DCOMWP5E-BLOCO-ENTRADA.
          10 DCOMWP5E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWP5E-ENTRADA.
             15 DCOMWP5E-CBCO                      PIC S9(003) COMP-3.
             15 DCOMWP5E-CAG-BCRIA                 PIC S9(005) COMP-3.
             15 DCOMWP5E-CCTA-BCRIA-CLI            PIC S9(013) COMP-3.
             15 DCOMWP5E-CPROT-SOLTC-AGNDA         PIC  X(050).
             15 DCOMWP5E-CTPO-PROT-AGNDA           PIC  X(001).
             15 DCOMWP5E-QTEMPO-VALDD-PROCM        PIC S9(003) COMP-3.
             15 DCOMWP5E-CSIT-PROT-AGNDA           PIC S9(001) COMP-3.
             15 DCOMWP5E-HINCL-REG                 PIC  X(026).
             15 DCOMWP5E-NSMULA-OPER-DESC          PIC S9(009) COMP-3.
             15 DCOMWP5E-DANO-OPER-DESC            PIC S9(004) COMP-3.
             15 DCOMWP5E-NSEQ-OPER-DESC            PIC S9(009) COMP-3.
             15 DCOMWP5E-FILLER                    PIC  X(189).
      *
          10 DCOMWP5E-BLOCO-VARIAVEL.
             15 DCOMWP5E-V-CTPO-CANAL              PIC S9(003) COMP-3.
             15 DCOMWP5E-V-QTEMPO-PROCM-CANAL      PIC S9(003) COMP-3.
             15 DCOMWP5E-V-FILLER                  PIC  X(296).
      *
          10 DCOMWP5E-BLOCO-PAGINACAO.
             15 DCOMWP5E-MAX-OCORR                 PIC  9(003).
             15 DCOMWP5E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWP5E-P-INICIAL              VALUE 'I'.
                88 DCOMWP5E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWP5E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWP5E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWP5E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWP5E-BLOCO-RESTART.
             15 DCOMWP5E-CHAVE-INI.
                20 DCOMWP5E-I-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWP5E-I-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWP5E-I-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWP5E-I-CPROT-SOLTC-AGNDA    PIC S9(009) COMP-3.
                20 DCOMWP5E-I-CTPO-PROT-AGNDA      PIC  X(001).
                20 DCOMWP5E-I-FILLER               PIC  X(020).
             15 DCOMWP5E-CHAVE-FIM.
                20 DCOMWP5E-F-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWP5E-F-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWP5E-F-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWP5E-F-CPROT-SOLTC-AGNDA    PIC S9(009) COMP-3.
                20 DCOMWP5E-F-CTPO-PROT-AGNDA      PIC  X(001).
                20 DCOMWP5E-F-FILLER               PIC  X(020).
      *
          10 DCOMWP5E-FILLER                       PIC  X(303).
      *
