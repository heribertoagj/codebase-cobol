       01  DCOMRB-REG.

           05  RB-DADOS-GERAIS.

      *--> IDENTIFICACAO DO CLIENTE
               10  RB-NOME-CLI                   PIC  X(40).
               10  RB-CBCO                       PIC S9(03)     COMP-3.
               10  RB-CAG-BCRIA                  PIC S9(05)     COMP-3.
               10  RB-CCTA-BCRIA-CLI             PIC S9(13)     COMP-3.
               10  FILLER                        PIC  X(28).

      *--> IDENTIFICACAO DA OPERACAO
               10  RB-DANO-OPER-DESC             PIC S9(04)     COMP-3.
               10  RB-NSEQ-OPER-DESC             PIC S9(09)     COMP-3.
               10  RB-CCONTR-CORP-OPER           PIC S9(07)     COMP-3.
               10  RB-CTPO-DESC-COML             PIC S9(03)     COMP-3.
               10  RB-CPRODT                     PIC S9(03)     COMP-3.
               10  RB-CSPROD-DESC-COML           PIC S9(03)     COMP-3.
               10  RB-CCONVE                     PIC S9(09)     COMP-3.
               10  RB-CVRSAO-CONVE               PIC S9(03)     COMP-3.
               10  RB-CMEIO-ENTRD-DESC           PIC S9(03)     COMP-3.
               10  RB-TIPO-PESSOA                PIC  X(01).
               10  RB-CAG-OPER-OPER              PIC S9(05)     COMP-3.
               10  RB-CELMTO-DESC-COML           PIC S9(3)      COMP-3.
               10  RB-DANO-BASE                  PIC S9(4)      COMP-3.
               10  RB-CDEPDC                     PIC S9(5)      COMP-3.
               10  RB-CSEQ-STUDO                 PIC S9(9)      COMP-3.
               10  RB-CCNPJ-CPF                  PIC S9(9)      COMP-3.
               10  RB-CFLIAL-CNPJ                PIC S9(5)      COMP-3.
               10  RB-CCTRL-CNPJ-CPF             PIC S9(2)      COMP-3.
               10  RB-CCART-VENCD-DESC           PIC X(5).
     ******** TIPO MODALIDADE DA BAIXA
               10  RB-TP-MOD-BAIXA               PIC X(02).
               10  RB-FILLER                     PIC X(19).

           05  RB-DADOS-BAIXA-COMANDADA.
               10  RB-NPCELA-DESC-COML           PIC S9(05)      COMP-3.
               10  RB-CCONTR-CORP-PCELA          PIC S9(07)      COMP-3.
     ********* T = TOTAL
     ********* P = PARCIAL
               10  RB-TPO-BAIXA                  PIC  X(01).
               10  RB-VALOR-BAIXA                PIC S9(15)V9(2) COMP-3.
               10  RB-CINDCD-LOC-DEB             PIC  X(01).
               10  RB-CAG-BCRIA-DEB              PIC S9(05)      COMP-3.
               10  RB-CCTA-BCRIA-DEB             PIC S9(13)      COMP-3.
               10  RB-CINDCD-BAIXA-COBR          PIC  X(01).
               10  RB-CINDCD-CONSL-EXTRT         PIC  X(01).
               10  RB-SISTEMA                    PIC  X(04).
               10  RB-CSIT-DESC-COML-ANT         PIC S9(03) COMP-3.
               10  RB-VPSENT                     PIC S9(15)V99 COMP-3.
               10  RB-VPRINC                     PIC S9(15)V99 COMP-3.
               10  RB-PERC-BAIXA                 PIC  9(03)V9(6) COMP-3.
               10  RB-CORIGE-PGTO-DESC           PIC  9(01).
               10  RB-FILLER                     PIC  X(20).

           05  RB-DADOS-BAIXA-POR-CHEQUE.
               10  RB-BAIXA-DE-CHEQUE            PIC  X(01).
               10  RB-CBCO-CHEQC                 PIC S9(03)      COMP-3.
               10  RB-CAG-BCRIA-CHEQC            PIC S9(05)      COMP-3.
               10  RB-CCCORR-CHEQC               PIC S9(13)      COMP-3.
               10  RB-CCHEQ-CHEQC                PIC S9(07)      COMP-3.
               10  RB-VSDO-DVDOR-PRINC           PIC S9(15)V9(2) COMP-3.
               10  RB-VJURO-MORA-LEGAL           PIC S9(15)V9(2) COMP-3.
               10  RB-VJURO-MORA-GERC            PIC S9(15)V9(2) COMP-3.
               10  RB-VIOF-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
               10  RB-VCUSTO-PCELA-VENCD         PIC S9(15)V9(2) COMP-3.
               10  RB-VJURO-TRNSF-CRATS          PIC S9(15)V9(2) COMP-3.
               10  RB-VCUSTO-TRNSF-CRATS         PIC S9(15)V9(2) COMP-3.
               10  RB-CCONTR-CORP-CHEQC          PIC S9(7)       COMP-3.
               10  RB-VR-CHEQC                   PIC S9(15)V9(2) COMP-3.
               10  RB-DTA-VCTO-CHEQC             PIC  9(09)      COMP-3.
               10  RB-CHQCD3-NRO-CNPJ-CPF-CHEQ   PIC 9(009)   COMP-3.
               10  RB-CHQCD3-FLIAL-CPNJ-CPF-CHEQ PIC 9(005)   COMP-3.
               10  RB-CHQCD3-CTRL-CNPJ-CPF-CHEQ  PIC 9(002).
               10  RB-CINDCD-BAIXA               PIC 9(001).
      *-->         RB-CINDCD-BAIXA = 1  -->  BAIXA DE OPERACAO
      *-->         RB-CINDCD-BAIXA = 2  -->  BAIXA DE PARCELA
               10  RB-CHQCD3-VLR-ATUALIZADO      PIC S9(15)V99 COMP-3.
               10  RB-CHQCD3-VLR-BAIXA           PIC S9(15)V99 COMP-3.
BSI001         10  RB-VJURO-REMUN-VENCD          PIC S9(15)V9(2) COMP-3.
BSI001         10  RB-VMORA-VENCD-DESC           PIC S9(15)V9(2) COMP-3.
BSI001         10  RB-VMULTA-VENCD-DESC          PIC S9(15)V9(2) COMP-3.
