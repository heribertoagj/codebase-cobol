      ******************************************************************
      * NOME BOOK : BVVEW011                                           *
      * DESCRICAO : AREA DE COMUNICACAO DO MODULO PARA OBTER LISTA DE  *
      *             CREDENCIADORAS E BANDEIRAS                         *
      * DATA      : 04/06/2019                                         *
      * AUTOR     : VITOR HUGO DE FREITAS SANTOS                       *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 211 BYTES                                          *
      ******************************************************************
       01  BVVEW011-AREA.
           05 BVVEW011-E-ENTRADA.
              10 BVVEW011-E-NCTRL-AUTRZ-AGNDA     PIC  9(011).
           05 BVVEW011-S-SAIDA.
              10 BVVEW011-S-CCNPJ-CPF-AUTRZ       PIC  9(009).
              10 BVVEW011-S-CFLIAL-CNPJ-AUTRZ     PIC  9(004).
              10 BVVEW011-S-CCTRL-CPF-AUTRZ       PIC  9(002).
              10 BVVEW011-S-AGENCIA               PIC  9(005).
              10 BVVEW011-S-CONTA                 PIC  9(013).
              10 BVVEW011-S-DOMICILIADO           PIC  X(001).
              10 BVVEW011-S-SOLICITACAO           PIC  9(001).
              10 BVVEW011-S-HENVIO-AUTRZ-AGNDA    PIC  X(026).
              10 BVVEW011-S-HRECEB-AUTRZ-AGNDA    PIC  X(026).
              10 BVVEW011-S-STATUS                PIC  9(001).
              10 BVVEW011-S-COD-ERRO              PIC  X(008).
              10 BVVEW011-S-COD-RETORNO           PIC  9(005).
              10 BVVEW011-S-MENSAGEM              PIC  X(079).
              10 BVVEW011-S-COD-CIP               PIC  X(020).
