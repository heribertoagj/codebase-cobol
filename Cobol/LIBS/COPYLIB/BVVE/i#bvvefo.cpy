      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEFO                                    *
      *  DESCRICAO.......: ATUALIZACAO DAS CONTAS COM TRAG             *
      *  TAMANHO.........: 100 BYTES                                   *
      ******************************************************************
      *                                                                *
       01  REG-BVVEFO.
           03 BVVEFO-NESTBL-AUTRZ-AGNDA  PIC S9(009)V   COMP-3.
           03 BVVEFO-AGENCIA-NOVA        PIC  9(005).
           03 BVVEFO-CONTA-NOVA          PIC  9(013).
           03 BVVEFO-AGENCIA-VELHA       PIC  9(005).
           03 BVVEFO-CONTA-VELHA         PIC  9(013).
           03 BVVEFO-CBANDE-VISAO        PIC S9(003)V   COMP-3.
           03 BVVEFO-CCRDRA-ESTBL-COML   PIC S9(004)V   COMP-3.
           03 BVVEFO-CCNPJ-CPF-AUTRZ     PIC S9(009)V   COMP-3.
           03 BVVEFO-CFLIAL-CNPJ-AUTRZ   PIC S9(005)V   COMP-3.
           03 BVVEFO-CCTRL-CPF-AUTRZ     PIC S9(002)V   COMP-3.
           03 FILLER                     PIC  X(044).
