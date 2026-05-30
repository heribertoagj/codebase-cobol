      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *                                                                *
      *  I#BVVEVB - DADOS DAS BUSCA/RETORNO POR CONTAS MOVIMENTO       *
      *  TAMANHO  : 1900                                               *
      *----------------------------------------------------------------*

       01  BVVEVB-REGISTRO.
           05 BVVEVB-AREA-CADU8421.
              10 BVVEVB-ENTRADA-CADU8421.
               15 BVVEVB-E-8421-MODULO          PIC  X(008).
               15 BVVEVB-E-8421-TP-ACESSO       PIC  X(011).
               15 BVVEVB-E-8421-CEMPR           PIC  9(005).
               15 BVVEVB-E-8421-CCUSTO          PIC  X(004).
               15 BVVEVB-E-8421-BANCO           PIC  9(003).
               15 BVVEVB-E-8421-AGENCIA         PIC  9(005).
               15 BVVEVB-E-8421-CONTA           PIC  9(007).
               15 BVVEVB-E-8421-CONTA-DIG       PIC  9(001).
               15 BVVEVB-E-8421-TP-PSSOA        PIC  X(001).
               15 BVVEVB-E-8421-CNPJ            PIC  9(015).
               15 BVVEVB-E-8421-NOME            PIC  X(070).
               15 BVVEVB-E-8421-DT-NASC         PIC  X(010).
               15 BVVEVB-E-8421-CLUB            PIC  9(010).
              10 BVVEVB-SAIDA-CADU8421.
               15 BVVEVB-S-8421-QTDE-OCORR      PIC  9(009).
               15 BVVEVB-S-8421-LISTA        OCCURS 50 TIMES.
                  20 BVVEVB-S-8421-CPF-CNPJ     PIC  X(015).
                  20 BVVEVB-S-8421-TP-PSSOA     PIC  X(001).
                  20 BVVEVB-S-8421-CLUB         PIC  9(010).
                  20 BVVEVB-S-8421-TITULARID    PIC  9(002).
                  20 BVVEVB-S-8421-SIT-PSSOA    PIC  9(003).
               15 BVVEVB-S-8421-RETORNO         PIC  9(002).
               15 BVVEVB-S-8421-MSG-ERRO        PIC  X(060).
      *
           05 BVVEVB-AREA-CADU8810     REDEFINES BVVEVB-AREA-CADU8421.
              10 BVVEVB-ENTRADA-CADU8810.
               15 BVVEVB-E-8810-MODULO          PIC  X(008).
               15 BVVEVB-E-8810-TP-ACESSO       PIC  X(011).
               15 BVVEVB-E-8810-CEMPR           PIC  9(005).
               15 BVVEVB-E-8810-CCUSTO-1        PIC  X(004).
               15 BVVEVB-E-8810-CLUB            PIC  9(010).
               15 BVVEVB-E-8810-CID-OPER.
                  20 BVVEVB-E-8810-CID          PIC  X(003).
                  20 BVVEVB-E-8810-CID-AG       PIC  X(005).
                  20 BVVEVB-E-8810-CID-CTA      PIC  X(007).
                  20 BVVEVB-E-8810-CID-RESTO    PIC  X(011).
               15 BVVEVB-E-8810-CEMPR-AG        PIC  9(005).
               15 BVVEVB-E-8810-AGENCIA         PIC  9(005).
               15 BVVEVB-E-8810-CCUSTO-2        PIC  X(004).
               15 BVVEVB-E-8810-TP-OBRIG        PIC  9(001).
               15 BVVEVB-E-8810-PROD-CRED       PIC  9(003).
               15 BVVEVB-E-8810-RAZAO           PIC  9(004).
               15 BVVEVB-E-8810-SEQUENCIA       PIC  9(003).
              10 BVVEVB-SAIDA-CADU8810.
               15 BVVEVB-S-8810-QTDE-OCORR      PIC  9(009).
               15 BVVEVB-S-8810-LISTA        OCCURS 10 TIMES.
                  20 BVVEVB-S-8810-CID-OPER.
                     25 BVVEVB-S-8810-CID       PIC  X(003).
                     25 BVVEVB-S-8810-CID-AG    PIC  X(005).
                     25 BVVEVB-S-8810-CID-CTA   PIC  X(007).
                     25 BVVEVB-S-8810-CID-RESTO PIC  X(011).
                  20 BVVEVB-S-8810-RAZAO        PIC  9(004).
               15 BVVEVB-S-8810-RETORNO         PIC  9(002).
               15 BVVEVB-S-8810-MSG-ERRO        PIC  X(060).
      *
           03 BVVEVB-AREA-SCCE9100     REDEFINES BVVEVB-AREA-CADU8421.
              05 BVVEVB-E-9100-MODULO          PIC  X(008).
              05 BVVEVB-E-9100-TP-ACESSO       PIC  X(012).
              05 BVVEVB-E-9100-AGENCIA         PIC  9(005).
              05 BVVEVB-E-9100-CONTA           PIC  9(007).
              05 BVVEVB-S-9100-RETORNO         PIC  9(002).
              05 BVVEVB-S-9100-MSG             PIC  X(030).
      *
           03 BVVEVB-AREA-SCMS8202     REDEFINES BVVEVB-AREA-CADU8421.
              05 BVVEVB-E-8202-MODULO          PIC  X(008).
              05 BVVEVB-E-8202-TP-ACESSO       PIC  X(012).
              05 BVVEVB-E-8202-CEMPR           PIC +9(005).
              05 BVVEVB-E-8202-AGENCIA         PIC +9(005).
              05 BVVEVB-E-8202-CONTA           PIC +9(013).
              05 BVVEVB-S-8202-RETORNO         PIC  9(002).
              05 BVVEVB-S-8202-SITUACAO        PIC +9(003).
      *
           03 BVVEVB-AREA-CLIB1013     REDEFINES BVVEVB-AREA-CADU8421.
              05 BVVEVB-E-1013-MODULO          PIC  X(008).
              05 BVVEVB-E-1013-TP-ACESSO       PIC  X(012).
              05 BVVEVB-E-1013-AGENCIA         PIC  9(005).
              05 BVVEVB-E-1013-CONTA           PIC  9(007).
              05 BVVEVB-S-1013-RETORNO         PIC  9(002).
              05 BVVEVB-S-1013-TP-BLOQ         PIC  X(001).
      *
           03 BVVEVB-RESERVA                   PIC  X(0129).
