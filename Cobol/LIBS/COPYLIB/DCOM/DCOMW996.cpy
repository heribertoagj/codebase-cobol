      ******************************************************************
      * NOME BOOK : DCOMW996                                           *
      * DESCRICAO : ROTEADOR CICS X IMS COM O RISC9996                 *
      * DATA      : 11/05/2021                                         *
      * AUTOR     : LUIS EDUARDO                                       *
      * EMPRESA   : BRQ                                                *
      * TAMANHO   : 01974 BYTES                                        *
      ******************************************************************
      * DCOMW996-E-CVERSAO             = CODIGO DA VERSAO DA BOOK      *
      * DCOMW996-E-CPROG-ORIGE         = CODIGO PROGRAMA ORIGEM        *
      * DCOMW996-E-CPROG-DSTNO         = CODIGO PROGRAMA DESTINO       *
      * DCOMW996-DADOS                 = AREA DE DADOS                 *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************************
           05 DCOMW996-IDENTIF-IC30.
              10   DCOMW996-IMS-TRANSACAO   PIC  X(008).
              10   DCOMW996-IDPGM           PIC  X(008).
           05 DCOMW996-IDENTIFICACAO.
              10   DCOMW996-E-CVERSAO       PIC  9(002).
              10   DCOMW996-E-TPO-CHAMADA   PIC  9(001).
              10   DCOMW996-E-CPROG-ORIGE   PIC  X(008).
              10   DCOMW996-E-CPROG-DSTNO   PIC  X(008).
           05 DCOMW996-DADOS.
              10   DCOMW996-AREA-ENTRADA.
                   15   DCOMW996-E-CEMPR-CPCAO        PIC  9(005).
                   15   DCOMW996-E-CCGC-CPF           PIC  9(009).
                   15   DCOMW996-E-CFILIAL-CGC        PIC  9(004).
                   15   DCOMW996-E-CCTRL-CPF-CGC      PIC  9(002).
                   15   DCOMW996-E-CTPO-CLI-RISCO     PIC  9(001).
                   15   DCOMW996-E-CCCUST-CPCAO       PIC  X(004).
                   15   DCOMW996-E-CAG-BCRIA-CPCAO    PIC  9(005).
                   15   DCOMW996-E-CCTA-BCRIA-CPCAO   PIC  9(013).
                   15   DCOMW996-E-CCART-CPCAO        PIC  X(003).
                   15   DCOMW996-E-CCONTR-RISCO-CPCAO PIC  9(017).
                   15   DCOMW996-E-CGRP-CTBIL-RZ      PIC  9(003).
                   15   DCOMW996-E-CSGRP-CTBIL-RZ     PIC  9(003).
              10   DCOMW996-AREA-SAIDA.
                   15   DCOMW996-COD-RETORNO          PIC  9(003).
                   15   DCOMW996-MSG-RETORNO          PIC  X(030).
                   15   DCOMW996-CCNPJ-CPF-RISCO      PIC  9(008).
                   15   DCOMW996-CMODLD-RZ-CTBIL      PIC  9(004).
                   15   DCOMW996-CTPO-CLI-RISCO       PIC  9(001).
                   15   DCOMW996-CCLI-RISCO-BACEN     PIC  X(014).
                   15   DCOMW996-CCCUST-CPCAO         PIC  X(004).
                   15   DCOMW996-CAG-BCRIA-CPCAO      PIC  9(005).
                   15   DCOMW996-CCTA-BCRIA-CPCAO     PIC  9(013).
                   15   DCOMW996-CCART-CPCAO          PIC  X(003).
                   15   DCOMW996-CCONTR-RISCO-CPCAO   PIC  9(017).
                   15   DCOMW996-WID-PDRAO-OPER       PIC  X(067).
              10   DCOMW996-RESERVA                   PIC  X(1362).
           05 DCOMW996-RESERVA                        PIC  X(005).
