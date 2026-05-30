      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEFQ                                    *
      *  DESCRICAO.......: ATUALIZACAO DAS CONTAS COM TRAG             *
      *  TAMANHO.........: 100 BYTES                                   *
      ******************************************************************
      *                                                                *
       01  REG-BVVEFQ.
           03 BVVEFQ-NCTRL-AUTRZ-AGNDA   PIC S9(011)V   COMP-3.
           03 BVVEFQ-AGENCIA-NOVA        PIC  9(005).
           03 BVVEFQ-CONTA-NOVA          PIC  9(013).
           03 BVVEFQ-AGENCIA-VELHA       PIC  9(005).
           03 BVVEFQ-CONTA-VELHA         PIC  9(013).
           03 BVVEFQ-CBANDE-VISAO        PIC S9(003)V   COMP-3.
           03 BVVEFQ-CCRDRA-ESTBL-COML   PIC S9(004)V   COMP-3.
           03 BVVEFQ-CCNPJ-CPF-AUTRZ     PIC S9(009)V   COMP-3.
           03 BVVEFQ-CFLIAL-CNPJ-AUTRZ   PIC S9(005)V   COMP-3.
           03 BVVEFQ-CCTRL-CPF-AUTRZ     PIC S9(002)V   COMP-3.
           03 FILLER                     PIC  X(043).
