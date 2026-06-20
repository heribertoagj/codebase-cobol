      *
      *****************************************************************
      * NOME DA INC - GFCTWBUG                                        *
      * DESCRICAO   - TARIFAS - IMAGEM DA BASE CADUB006 - TELEFONES   *
      * TAMANHO     - 0642                                            *
      * DATA        - JUN/2013                                        *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  BUG-REGISTRO.
             05  BUG-CPF-CNPJ.
                 07  BUG-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                 07  BUG-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                 07  BUG-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
             05  BUG-CCLUB                PIC S9(010) COMP-3.
             05  BUG-TP-REG               PIC  X(003).
             05  BUG-CSEQ-CNTRL-FONE      PIC S9(003) COMP-3.
             05  BUG-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
             05  BUG-CTPO-FONE-CLI        PIC S9(003) COMP-3.
             05  BUG-CTPO-LIN-TFONI       PIC S9(003) COMP-3.
             05  BUG-CINDCD-FONE-INVLD    PIC  X(001).
             05  BUG-CFONE                PIC S9(008) COMP-3.
             05  BUG-CID-RMAL-BIP-PAGER   PIC  X(015).
             05  BUG-ICNTAT-PSSOA         PIC  X(020).
             05  BUG-CDDDI                PIC  X(004).
             05  BUG-CDDDD                PIC  X(004).
             05  BUG-CEMPR                PIC S9(010) COMP-3.
             05  BUG-CAUTRZ-ENVIO-SMS     PIC  X(001).
             05  BUG-CESPCE-ENDER         PIC S9(001) COMP-3.
             05  BUG-CTPO-FONE            PIC S9(002) COMP-3.
             05  BUG-CNVEL-CONFD-ENDER    PIC S9(003) COMP-3.
             05  BUG-CCOMPV-ENDER-PSSOA   PIC S9(003) COMP-3.
             05  BUG-DINIC-UTILZ-ENDER    PIC  X(010).
             05  BUG-DFIM-UTILZ-ENDER     PIC  X(010).
             05  BUG-IPSSOA-CNTAT-ENDER   PIC  X(040).
             05  BUG-CENDER-PDRAO-PSSOA   PIC  X(001).
             05  BUG-CUSO-ENDER-PSSOA     PIC S9(003) COMP-3.
             05  BUG-ICARGO-CNTAT         PIC  X(040).
             05  BUG-CCLUB-ENDER-PSSOA    PIC S9(010) COMP-3.
             05  BUG-CEMPR-ENDER-PSSOA    PIC S9(010) COMP-3.
             05  BUG-CSEQ-ENDER-PSSOA     PIC S9(005) COMP-3.
             05  BUG-CCARGO-CLI-PF        PIC S9(005) COMP-3.
             05  BUG-NLIN-TFONI           PIC S9(011) COMP-3.
             05  BUG-STATUS               PIC  X(01).
             05  BUG-RESERVA              PIC  X(424).
             05  BUG-CPF-CNPJ-ST.
ST2507           07  BUG-CPF-CNPJ-NRO-S   PIC  X(009).
ST2507           07  BUG-CPF-CNPJ-CTR-S   PIC S9(002) COMP-3.
ST2507           07  BUG-CPF-CNPJ-FIL-S   PIC  X(004).
