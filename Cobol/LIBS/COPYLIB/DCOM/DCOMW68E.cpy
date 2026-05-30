      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB068: DB2PRD.TSPROD_REGRA_OPER                      *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM468S                                       *
      *    -> FETCH   - DCOM468F                                       *
      *    -> INSERT  - DCOM468I                                       *
      *    -> UPDATE  - DCOM468U                                       *
      *    -> DELETE  - DCOM468D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW68-HEADER.
          10 DCOMW68E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW68E'.
          10 DCOMW68E-TAM-LAYOUT       PIC  9(005) VALUE 00718.
      *
       05 DCOMW68E-BLOCO-ENTRADA.
          10 DCOMW68E-INSTRUCAO                    PIC  X(010).
          10 DCOMW68E-ENTRADA.
             15 DCOMW68E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMW68E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMW68E-HINIC-REGRA-OPER          PIC  X(026).
             15 DCOMW68E-HFIM-REGRA-OPER           PIC  X(026).
             15 DCOMW68E-CINDCD-CALC-JURO          PIC  X(001).
             15 DCOMW68E-CINDCD-PGDOR-JURO         PIC  X(001).
             15 DCOMW68E-CINDCD-TX-REDE            PIC  X(001).
             15 DCOMW68E-CINDCD-INCID-IOF          PIC  X(001).
             15 DCOMW68E-CINDCD-DESC-SACDO         PIC  X(001).
             15 DCOMW68E-CINDCD-NEGOC-DIFER        PIC  X(001).
             15 DCOMW68E-VMIN-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMW68E-VMAX-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMW68E-CREGRA-MORA-LEGAL         PIC S9(003) COMP-3.
             15 DCOMW68E-CREGRA-GERC-CONS          PIC S9(003) COMP-3.
             15 DCOMW68E-CREGRA-GERC-MORA          PIC S9(003) COMP-3.
             15 DCOMW68E-CREGRA-GERC-CRATS         PIC S9(003) COMP-3.
             15 DCOMW68E-HULT-ATULZ                PIC  X(026).
             15 DCOMW68E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMW68E-CTERM                     PIC  X(008).
             15 DCOMW68E-CINDCD-PROTE-TITLO        PIC  X(001).
             15 DCOMW68E-QDIA-PROTE-TITLO          PIC S9(003) COMP-3.
             15 DCOMW68E-CINDCD-DIA-PROTE          PIC  X(002).
             15 DCOMW68E-HINIC-REG-BACEN           PIC  X(008).
             15 DCOMW68E-HFIM-REG-BACEN            PIC  X(008).
             15 DCOMW68E-CREG-OPER-BACEN           PIC  X(001).
             15 DCOMW68E-CINDCD-REG-AMBTL          PIC  X(001).
             15 DCOMW68E-QDIA-CALC-JURO            PIC S9(003) COMP-3.
          10 DCOMW68E-BLOCO-VARIAVEL.
             15 DCOMW68E-V-FILLER                  PIC  X(300).
          10 DCOMW68E-BLOCO-PAGINACAO.
             15 DCOMW68E-MAX-OCORR                 PIC  9(003).
             15 DCOMW68E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW68E-P-INICIAL              VALUE 'I'.
                88 DCOMW68E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW68E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW68E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW68E-P-ULTIMA               VALUE 'U'.
          10 DCOMW68E-BLOCO-RESTART.
             15 DCOMW68E-CHAVE-INI.
                20 DCOMW68E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW68E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW68E-I-HINIC-REGRA-OPER     PIC  X(026).
             15 DCOMW68E-CHAVE-FIM.
                20 DCOMW68E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW68E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW68E-F-HINIC-REGRA-OPER     PIC  X(026).
          10 DCOMW68E-FILLER                       PIC  X(179).
      *
