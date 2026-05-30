      ***===========================================================***
      *** NOME INC                                     LENGTH=00322 ***
      *** I#SLIG71                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO REGS.SELECIONADOS VENDOR - COMPRADOR   ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-SELECIONADO.
           03  SLIG71-CTRL.
               05  SLIG71-CGC-CPF-CTRL-ST.
                   07  SLIG71-CGC-CPF-CTRL       PIC 9(009).
               05  SLIG71-PRODUTO-CTRL           PIC X(001).
               05  SLIG71-CARTEIRA-CTRL          PIC 9(001).
               05  SLIG71-CD-PRODUTO-CTRL        PIC 9(002).
               05  SLIG71-CD-CNEGOC-CTRL         PIC 9(011).
               05  SLIG71-CD-NOSNRO-CTRL         PIC 9(011).
               05  SLIG71-DT-ENTRADA-CTRL        PIC 9(008).
               05  SLIG71-DT-VENCTO-PARC-CTRL    PIC 9(008).
           03  SLIG71-IDENT-DOCTO.
               05  SLIG71-CPRODT-CLI             PIC 9(003).
               05  FILLER                        PIC X(001).
               05  SLIG71-CFAML-CLI              PIC 9(001).
               05  FILLER                        PIC X(001).
               05  SLIG71-CCONTR-CLI             PIC 9(009).
               05  FILLER                        PIC X(001).
               05  SLIG71-CSEQ-CLI               PIC 9(003).
               05  FILLER                        PIC X(001).
               05  SLIG71-CPARCELA-CLI           PIC 9(003).
               05  FILLER                        PIC X(001).
               05  SLIG71-NUM-DOCTO              PIC 9(011).
           03 SLIG71-PERIODO.
               05  SLIG71-DT-PER-INI             PIC 9(008).
               05  SLIG71-DT-PER-FIM             PIC 9(008).
           03  SLIG71-VINCULACAO.
               05 SLIG71-DIR-REG                 PIC 9(005) COMP-3.
               05 SLIG71-GER-REG                 PIC 9(005) COMP-3.
               05 SLIG71-GRP-ECONM               PIC 9(009) COMP-3.
               05 SLIG71-AGENCIA                 PIC 9(005) COMP-3.
           03 SLIG71-REG-VENDOR.
               05 SLIG71-CGC-CPF-COMP            PIC 9(009) COMP-3.
               05 SLIG71-FILIAL-COMP             PIC 9(004) COMP-3.
               05 SLIG71-CONTROLE-COMP           PIC 9(002) COMP-3.
               05 SLIG71-TIPO-PESSOA-COMP        PIC X(001).
               05 SLIG71-BCO-COMP                PIC 9(003) COMP-3.
               05 SLIG71-AGENCIA-COMP            PIC 9(005) COMP-3.
               05 SLIG71-CONTA-COMP              PIC 9(013) COMP-3.
               05 SLIG71-NOME-COMP               PIC X(040).
               05 SLIG71-CPRODT                  PIC 9(003) COMP-3.
               05 SLIG71-CFAML                   PIC 9(001) COMP-3.
               05 SLIG71-CCONTR                  PIC 9(009) COMP-3.
               05 SLIG71-CSEQ                    PIC 9(003) COMP-3.
               05 SLIG71-CPARCELA                PIC 9(003) COMP-3.
               05 SLIG71-NUM-PAGTO               PIC 9(009) COMP-3.
               05 SLIG71-NOSSO-NUMERO            PIC 9(011) COMP-3.
               05 SLIG71-CGC-CPF-FORN            PIC 9(009) COMP-3.
               05 SLIG71-FILIAL-FORN             PIC 9(004) COMP-3.
               05 SLIG71-CONTROLE-FORN           PIC 9(002) COMP-3.
               05 SLIG71-TIPO-PESSOA-FORN        PIC X(001).
               05 SLIG71-BCO-FORN                PIC 9(003) COMP-3.
               05 SLIG71-AGENCIA-FORN            PIC 9(005) COMP-3.
               05 SLIG71-CONTA-FORN              PIC 9(013) COMP-3.
               05 SLIG71-NOME-FORN               PIC X(040).
               05 SLIG71-DT-VENCTO-PARC          PIC 9(008).
               05 SLIG71-DT-VENCTO-PARC-RED REDEFINES
                  SLIG71-DT-VENCTO-PARC.
                  10 SLIG71-ANO-VENCTO-PARC      PIC 9(004).
                  10 SLIG71-MES-VENCTO-PARC      PIC 9(002).
                  10 SLIG71-DIA-VENCTO-PARC      PIC 9(002).
               05 SLIG71-DT-PAGTO-PARC           PIC 9(008).
               05 SLIG71-DT-PAGTO-PARC-RED REDEFINES
                  SLIG71-DT-PAGTO-PARC.
                  10 SLIG71-ANO-PAGTO-PARC       PIC 9(004).
                  10 SLIG71-MES-PAGTO-PARC       PIC 9(002).
                  10 SLIG71-DIA-PAGTO-PARC       PIC 9(002).
               05 SLIG71-CPTO-SIT-PARC           PIC 9(003) COMP-3.
                  88 SLIG71-CTPO-PAGTO-NORMAL    VALUE 1.
                  88 SLIG71-CTPO-PAGTO-ANTECI    VALUE 2.
                  88 SLIG71-CTPO-BAIXA-NORMAL    VALUE 3.
                  88 SLIG71-CTPO-BAIXA-ANTECI    VALUE 4.
                  88 SLIG71-CTPO-BAIXA-EXCLUS    VALUE 5.
                  88 SLIG71-CTPO-SIT-PARC-VENC   VALUE 6.
                  88 SLIG71-CTPO-SIT-PARC-FUTU   VALUE 7.
               05 SLIG71-DT-ENTRADA-ORIGEM       PIC 9(008).
               05 SLIG71-DT-ENTRADA-ORIGEM-RED REDEFINES
                  SLIG71-DT-ENTRADA-ORIGEM.
                  10 SLIG71-ANO-ENTRADA-ORIGEM   PIC 9(004).
                  10 SLIG71-MES-ENTRADA-ORIGEM   PIC 9(002).
                  10 SLIG71-DIA-ENTRADA-ORIGEM   PIC 9(002).
               05 SLIG71-VL-PRINC-PARC           PIC 9(013)V99 COMP-3.
               05 SLIG71-IND-PAGTO               PIC X(001).
                  88 SLIG71-IND-PAGTO-COMP       VALUE '1'.
                  88 SLIG71-IND-PAGTO-FORN       VALUE '2'.
               05 SLIG71-BCO-DIR-REGIONAL        PIC 9(003) COMP-3.
               05 SLIG71-DIR-REGIONAL            PIC 9(005) COMP-3.
               05 SLIG71-BCO-GERC-REGIONAL       PIC 9(003) COMP-3.
               05 SLIG71-GERC-REGIONAL           PIC 9(005) COMP-3.
               05 SLIG71-GRP-ECONOMICO-COMP      PIC 9(009) COMP-3.
               05 SLIG71-GRP-ECONOMICO-FORN      PIC 9(009) COMP-3.
               05 SLIG71-CGC-CPF-COMP-ST         PIC X(009).
               05 SLIG71-FILIAL-COMP-ST          PIC X(004).
               05 SLIG71-CONTROLE-COMP-ST        PIC 9(002) COMP-3.
               05 SLIG71-CGC-CPF-FORN-ST         PIC X(009).
               05 SLIG71-FILIAL-FORN-ST          PIC X(004).
               05 SLIG71-CONTROLE-FORN-ST        PIC 9(002) COMP-3.
               05 SLIG71-FILLER                  PIC X(002).
