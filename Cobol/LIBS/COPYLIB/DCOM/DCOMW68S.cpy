      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB068: DB2PRD.TSPROD_REGRA_OPER                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM468S                                       *
      *    -> FETCH   - DCOM468F                                       *
      *    -> INSERT  - DCOM468I                                       *
      *    -> UPDATE  - DCOM468U                                       *
      *    -> DELETE  - DCOM468D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW68S-HEADER.
          10 DCOMW68S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMW68S'.
          10 DCOMW68S-TAM-LAYOUT   PIC  9(005) VALUE 07800.
      *
          10 DCOMW68S-BLOCO-SAIDA.
             15 DCOMW68S-QTDE-TOTAL            PIC  9(010).
             15 DCOMW68S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMW68S-OCORRENCIA            OCCURS 050 TIMES.
                20 DCOMW68S-CPRODT             PIC S9(003) COMP-3.
                20 DCOMW68S-CSPROD-DESC-COML   PIC S9(003) COMP-3.
                20 DCOMW68S-HINIC-REGRA-OPER   PIC  X(026).
                20 DCOMW68S-HFIM-REGRA-OPER    PIC  X(026).
                20 DCOMW68S-CINDCD-CALC-JURO   PIC  X(001).
                20 DCOMW68S-CINDCD-PGDOR-JURO  PIC  X(001).
                20 DCOMW68S-CINDCD-TX-REDE     PIC  X(001).
                20 DCOMW68S-CINDCD-INCID-IOF   PIC  X(001).
                20 DCOMW68S-CINDCD-DESC-SACDO  PIC  X(001).
                20 DCOMW68S-CINDCD-NEGOC-DIFER PIC  X(001).
                20 DCOMW68S-VMIN-OPER-DESC     PIC S9(015)V9(002)
                                                            COMP-3.
                20 DCOMW68S-VMAX-OPER-DESC     PIC S9(015)V9(002)
                                                            COMP-3.
                20 DCOMW68S-CREGRA-MORA-LEGAL  PIC S9(003) COMP-3.
                20 DCOMW68S-CREGRA-GERC-CONS   PIC S9(003) COMP-3.
                20 DCOMW68S-CREGRA-GERC-MORA   PIC S9(003) COMP-3.
                20 DCOMW68S-CREGRA-GERC-CRATS  PIC S9(003) COMP-3.
                20 DCOMW68S-HULT-ATULZ         PIC  X(026).
                20 DCOMW68S-CFUNC-BDSCO        PIC S9(009) COMP-3.
                20 DCOMW68S-CTERM              PIC  X(008).
                20 DCOMW68S-CINDCD-PROTE-TITLO PIC  X(001).
                20 DCOMW68S-QDIA-PROTE-TITLO   PIC S9(003) COMP-3.
                20 DCOMW68S-CINDCD-DIA-PROTE   PIC  X(002).
                20 DCOMW68S-HINIC-REG-BACEN    PIC  X(008).
                20 DCOMW68S-HFIM-REG-BACEN     PIC  X(008).
                20 DCOMW68S-CREG-OPER-BACEN    PIC  X(001).
                20 DCOMW68S-CINDCD-REG-AMBTL   PIC  X(001).
                20 DCOMW68S-QDIA-CALC-JURO     PIC S9(003) COMP-3.
             15 DCOMW68S-FILLER                PIC  X(174).
      *
