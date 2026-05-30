      *================================================================*
      *    INC = I#DCOMXB                                              *
      *----------------------------------------------------------------*
      *    AREA DE COMUNICACAO DO MODULO DCOM5297                      *
      *    OBTER DADOS DA SIMULACAO                                    *
      *================================================================*

       01  5297-ENTRADA.
           05 5297E-COD-RETORNO        PIC  X(004).
           05 5297E-MSG-RETORNO        PIC  X(079).
           05 5297E-RESTART            PIC  9(005).
           05 5297E-FLAG               PIC  X(001).
           05 5297E-CFUNC-ENV          PIC  9(009).
           05 5297E-CTERM              PIC  X(008).
           05 5297E-NSMULA-OPER-DESC   PIC  9(009).

       01  5297-SAIDA.
           05 5297S-COD-RETORNO        PIC  X(004).
           05 5297S-MSG-RETORNO        PIC  X(079).
           05 5297S-IRSUMO-SIT-DESC    PIC  X(015).
           05 5297S-DINIC-OPER-DESC    PIC  X(010).
           05 5297S-CSIT-DESC-COML     PIC  9(003).
           05 5297S-HSIT-DESC-COML     PIC  X(026).
           05 5297S-CPRODT             PIC  9(003).
           05 5297S-IPRODT             PIC  X(060).
           05 5297S-IABREV-PRODT       PIC  X(012).
           05 5297S-CSPROD-DESC-COML   PIC  9(003).
           05 5297S-ISPROD-DESC-COML   PIC  X(040).
           05 5297S-IRSUMO-SPROD-DESC  PIC  X(010).
           05 5297S-CTPO-DESC-COML     PIC  9(003).
           05 5297S-ITPO-DESC-COML     PIC  X(030).
           05 5297S-IRSUMO-TPO-DESC    PIC  X(010).
           05 5297S-CBCO               PIC  9(003).
           05 5297S-IBCO               PIC  X(040).
           05 5297S-CAG-BCRIA          PIC  9(005).
           05 5297S-IAG-BCRIA          PIC  X(040).
           05 5297S-CCTA-BCRIA-CLI     PIC  9(013).
           05 5297S-CCNPJ-CPF          PIC  9(009).
           05 5297S-CFLIAL-CNPJ        PIC  9(004).
           05 5297S-CCTRL-CNPJ-CPF     PIC  9(002).
           05 5297S-IPSSOA-DESC-COML   PIC  X(060).
           05 5297S-CBCO-OPER-OPER     PIC  9(003).
           05 5297S-CAG-OPER-OPER      PIC  9(005).
           05 5297S-CCONTR-CONVE-DESC  PIC  9(009).
           05 5297S-CVRSAO-CONTR-CONVE PIC  9(003).
           05 5297S-CELMTO-DESC-COML   PIC  9(003).
           05 5297S-CINDCD-DIGTC-AG    PIC  X(001).
           05 5297S-CINDCD-MODLD-INCL  PIC  X(001).
           05 5297S-CCTRO-CUSTO        PIC  X(004).
           05 5297S-VOPER-DESC-COML    PIC  9(015)V9(002).
           05 5297S-TOPER-DESC-APURA   PIC  9(003).
           05 5297S-VOPER-DESC-APURA   PIC  9(015)V9(002).
           05 5297S-QTITLO-OPER-APURA  PIC  9(005).
           05 5297S-DVCTO-FNAL-APURA   PIC  X(010).
           05 5297S-TMED-PONDE-OPER    PIC  9(003).
           05 5297S-CMEIO-ENTRD-DESC   PIC  9(003).
           05 5297S-IRSUMO-MEIO-ENTRD  PIC  X(015).
           05 5297S-CTPO-TX-JURO-DESC  PIC  9(003).
           05 5297S-ITPO-TX-JURO-DESC  PIC  X(030).
           05 5297S-IRSUMO-TPO-TX-JURO PIC  X(015).
           05 5297S-CSGL-TPO-TX-JURO   PIC  X(006).
           05 5297S-PALIQT-IOF-DESC    PIC  9(003)V9(005).
           05 5297S-PTX-JURO-MES       PIC  9(003)V9(005).
           05 5297S-PTX-JURO-ANO       PIC  9(003)V9(005).
           05 5297S-VJURO-OPER-DESC    PIC  9(015)V9(002).
           05 5297S-VTARIF-REG-TITLO   PIC  9(015)V9(002).
           05 5297S-VTAC-OPER-DESC     PIC  9(015)V9(002).
           05 5297S-VIOF-OPER-DESC     PIC  9(015)V9(002).
           05 5297S-VLIQ-OPER-DESC     PIC  9(015)V9(002).
           05 5297S-HULT-ATULZ         PIC  X(026).
           05 5297S-CFUNC-BDSCO        PIC  9(009).
           05 5297S-CTERM              PIC  X(008).
           05 5297S-DINCL-REG          PIC  X(010).
           05 5297S-CCHAVE-ELMTO-DESC  PIC  X(045).
           05 5297S-PTX-CUSTO-SPREAD   PIC  9(003)V9(003).
           05 5297S-QTITLO-CHEQ-PCELA  PIC  9(009).
           05 5297S-CINDCD-ALIQT-REDZD PIC  X(001).
           05 5297S-PTX-JURO-DIA       PIC  9(003)V9(005).
           05 5297S-CINDCD-FREQ-TX     PIC  X(002).
           05 5297S-PRAZO-FLUXO        PIC  9(003).
