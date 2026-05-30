      ******************************************************************
      * NOME BOOK : BVVEW012                                           *
      * DESCRICAO : AREA DE COMUNICACAO DO MODULO PARA OBTER LISTA DE  *
      *             LOG DE PEDIDOS - OPT-IN/OPT-OUT.                   *
      * DATA      : 27/06/2019                                         *
      * AUTOR     : FLAVIO AUGUSTO MARIA                               *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 2087 BYTES                                         *
      * -------------------------------------------------------------- *
      *        CAMPO                       DESCRICAO                   *
      * -------------------------------------------------------------- *
      * BVVEW012-S-CTPO-ARQ-AUTRZ:   TIPO DA SOLICITACAO               *
      *                              1 - OPT-IN                        *
      *                              2 - OPT-OUT                       *
      *                              3 - VARREDURA                     *
      * BVVEW012-S-CSIT-PROCM-AUTRZ: SITUACAO DO REGISTRO NA TABELA    *
      *                              0 - A PROCESSAR                   *
      *                              1 - ENVIADO                       *
      *                              2 - PROCESSADO COM SUCESSO        *
      *                              3 - NAO OK                        *
      ******************************************************************
       01  BVVEW012-AREA.
           05 BVVEW012-E-ENTRADA.
              10 BVVEW012-E-QTDE-OCORR               PIC  9(005).
              10 BVVEW012-E-PAGINA                   PIC  9(003).
              10 BVVEW012-E-CPF-CNPJ.
                 15 BVVEW012-E-CCPF                  PIC  9(009).
                 15 BVVEW012-E-CPF-FILIAL            PIC  9(004).
                 15 BVVEW012-E-CPF-DIG               PIC  9(002).
              10 BVVEW012-E-AGENCIA                  PIC  9(005).
              10 BVVEW012-E-CONTA                    PIC  9(007).
              10 BVVEW012-E-DIG-CONTA                PIC  9(001).
           05 BVVEW012-S-SAIDA.
              10 BVVEW012-S-COD-RETORNO              PIC  9(005).
              10 BVVEW012-S-QTDE-OCORR               PIC  9(005).
              10 BVVEW012-S-FIM-DADOS                PIC  X(001).
              10 BVVEW012-S-MENSAGEM                 PIC  X(079).
              10 BVVEW012-S-DADOS OCCURS 11 TIMES.
                 15 BVVEW012-S-NCTRL-AUTRZ-AGNDA     PIC  9(011).
                 15 BVVEW012-S-CPF-CNPJ.
                    20 BVVEW012-S-CCPF               PIC  9(009).
                    20 BVVEW012-S-CPF-FILIAL         PIC  9(004).
                    20 BVVEW012-S-CPF-DIG            PIC  9(002).
                 15 BVVEW012-S-CCRDRA-ESTBL-COML     PIC  9(004).
                 15 BVVEW012-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                 15 BVVEW012-S-CBANDE-VISAO          PIC  9(003).
                 15 BVVEW012-S-RBANDE-VISAO          PIC  X(060).
                 15 BVVEW012-S-CTPO-ARQ-AUTRZ        PIC  9(001).
                 15 BVVEW012-S-CSIT-PROCM-AUTRZ      PIC  9(001).
                 15 BVVEW012-S-HRECEB-AUTRZ-AGNDA    PIC  X(026).
           05 BVVEW012-S-RESERVA.
              10 FILLER                              PIC  X(300).
