      ***===========================================================***
      *** NOME INC                                     LENGTH=00308 ***
      *** I#SLIG55                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO REGS.SELECIONADOS VENDOR - COMPRADOR   ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-SELECIONADO.
           03  SLIG55-CTRL.
               05  SLIG55-CGC-CPF-CTRL           PIC X(009).
               05  SLIG55-PRODUTO-CTRL           PIC X(001).
               05  SLIG55-CARTEIRA-CTRL          PIC 9(001).
               05  SLIG55-CD-PRODUTO-CTRL        PIC 9(002).
               05  SLIG55-CD-CNEGOC-CTRL         PIC 9(011).
               05  SLIG55-CD-NOSNRO-CTRL         PIC 9(011).
               05  SLIG55-DT-ENTRADA-CTRL        PIC 9(008).
               05  SLIG55-DT-VENCTO-PARC-CTRL    PIC 9(008).
           03  SLIG55-IDENT-DOCTO.
               05  SLIG55-CPRODT-CLI             PIC 9(003).
               05  FILLER                        PIC X(001).
               05  SLIG55-CFAML-CLI              PIC 9(001).
               05  FILLER                        PIC X(001).
               05  SLIG55-CCONTR-CLI             PIC 9(009).
               05  FILLER                        PIC X(001).
               05  SLIG55-CSEQ-CLI               PIC 9(003).
               05  FILLER                        PIC X(001).
               05  SLIG55-CPARCELA-CLI           PIC 9(003).
               05  FILLER                        PIC X(001).
               05  SLIG55-NUM-DOCTO              PIC 9(011).
           03 SLIG55-PERIODO.
               05  SLIG55-DT-PER-INI             PIC 9(008).
               05  SLIG55-DT-PER-FIM             PIC 9(008).
           03 SLIG55-REG-VENDOR.
               05 SLIG55-CGC-CPF-COMP            PIC 9(009) COMP-3.
               05 SLIG55-FILIAL-COMP             PIC 9(004) COMP-3.
               05 SLIG55-CONTROLE-COMP           PIC 9(002) COMP-3.
               05 SLIG55-TIPO-PESSOA-COMP        PIC X(001).
               05 SLIG55-BCO-COMP                PIC 9(003) COMP-3.
               05 SLIG55-AGENCIA-COMP            PIC 9(005) COMP-3.
               05 SLIG55-CONTA-COMP              PIC 9(013) COMP-3.
               05 SLIG55-NOME-COMP               PIC X(040).
               05 SLIG55-CPRODT                  PIC 9(003) COMP-3.
               05 SLIG55-CFAML                   PIC 9(001) COMP-3.
               05 SLIG55-CCONTR                  PIC 9(009) COMP-3.
               05 SLIG55-CSEQ                    PIC 9(003) COMP-3.
               05 SLIG55-CPARCELA                PIC 9(003) COMP-3.
               05 SLIG55-NUM-PAGTO               PIC 9(009) COMP-3.
               05 SLIG55-NOSSO-NUMERO            PIC 9(011) COMP-3.
               05 SLIG55-CGC-CPF-FORN            PIC 9(009) COMP-3.
               05 SLIG55-FILIAL-FORN             PIC 9(004) COMP-3.
               05 SLIG55-CONTROLE-FORN           PIC 9(002) COMP-3.
               05 SLIG55-TIPO-PESSOA-FORN        PIC X(001).
               05 SLIG55-BCO-FORN                PIC 9(003) COMP-3.
               05 SLIG55-AGENCIA-FORN            PIC 9(005) COMP-3.
               05 SLIG55-CONTA-FORN              PIC 9(013) COMP-3.
               05 SLIG55-NOME-FORN               PIC X(040).
               05 SLIG55-DT-VENCTO-PARC          PIC 9(008).
               05 SLIG55-DT-VENCTO-PARC-RED REDEFINES
                  SLIG55-DT-VENCTO-PARC.
                  10 SLIG55-ANO-VENCTO-PARC      PIC 9(004).
                  10 SLIG55-MES-VENCTO-PARC      PIC 9(002).
                  10 SLIG55-DIA-VENCTO-PARC      PIC 9(002).
               05 SLIG55-DT-PAGTO-PARC           PIC 9(008).
               05 SLIG55-DT-PAGTO-PARC-RED REDEFINES
                  SLIG55-DT-PAGTO-PARC.
                  10 SLIG55-ANO-PAGTO-PARC       PIC 9(004).
                  10 SLIG55-MES-PAGTO-PARC       PIC 9(002).
                  10 SLIG55-DIA-PAGTO-PARC       PIC 9(002).
               05 SLIG55-CPTO-SIT-PARC           PIC 9(003) COMP-3.
                  88 SLIG55-CTPO-PAGTO-NORMAL    VALUE 1.
                  88 SLIG55-CTPO-PAGTO-ANTECI    VALUE 2.
                  88 SLIG55-CTPO-BAIXA-NORMAL    VALUE 3.
                  88 SLIG55-CTPO-BAIXA-ANTECI    VALUE 4.
                  88 SLIG55-CTPO-BAIXA-EXCLUS    VALUE 5.
                  88 SLIG55-CTPO-SIT-PARC-VENC   VALUE 6.
                  88 SLIG55-CTPO-SIT-PARC-FUTU   VALUE 7.
               05 SLIG55-DT-ENTRADA-ORIGEM       PIC 9(008).
               05 SLIG55-DT-ENTRADA-ORIGEM-RED REDEFINES
                  SLIG55-DT-ENTRADA-ORIGEM.
                  10 SLIG55-ANO-ENTRADA-ORIGEM   PIC 9(004).
                  10 SLIG55-MES-ENTRADA-ORIGEM   PIC 9(002).
                  10 SLIG55-DIA-ENTRADA-ORIGEM   PIC 9(002).
               05 SLIG55-VL-PRINC-PARC           PIC 9(013)V99 COMP-3.
               05 SLIG55-IND-PAGTO               PIC X(001).
                  88 SLIG55-IND-PAGTO-COMP       VALUE '1'.
                  88 SLIG55-IND-PAGTO-FORN       VALUE '2'.
               05 SLIG55-BCO-DIR-REGIONAL        PIC 9(003) COMP-3.
               05 SLIG55-DIR-REGIONAL            PIC 9(005) COMP-3.
               05 SLIG55-BCO-GERC-REGIONAL       PIC 9(003) COMP-3.
               05 SLIG55-GERC-REGIONAL           PIC 9(005) COMP-3.
               05 SLIG55-GRP-ECONOMICO-COMP      PIC 9(009) COMP-3.
               05 SLIG55-GRP-ECONOMICO-FORN      PIC 9(009) COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
               05 SLIG55-CGC-CPF-COMP-ST         PIC X(009).
               05 SLIG55-FILIAL-COMP-ST          PIC X(004).
               05 SLIG55-CONTROLE-COMP-ST        PIC 9(002) COMP-3.
               05 SLIG55-CGC-CPF-FORN-ST         PIC X(009).
               05 SLIG55-FILIAL-FORN-ST          PIC X(004).
               05 SLIG55-CONTROLE-FORN-ST        PIC 9(002) COMP-3.
               05 SLIG55-FILLER                  PIC X(002).
