      *****************************************************************
      *               COMUNICACAO WEB X IMS                 - ENTRADA *
      * NOME DA INC - GFCTWAPE                                        *
      * DESCRICAO   - GESTAO - DEMONSTRATIVO DE EVENTOS DO CLIENTE    *
      *                        LISTA DE IMPEDIMENTOS       - GFCT5302 *
      * TAMANHO     - 150                                             *
      * DATA        - NOV/2010                                        *
      * RESPONSAVEL - FRANCISCO LOURENCO              - SONDAPROCWORK *
      *****************************************************************
      *
       01  WAPE-ENTRADA.
           03  WAPE-LL                      PIC S9(004) COMP.
           03  WAPE-ZZ                      PIC S9(004) COMP.
           03  WAPE-TRANSACAO               PIC  X(009).
           03  WAPE-FUNCAO                  PIC  X(008).
           03  WAPE-DADOS-RST.
               05  WAPE-FILTRO.
                   07  WAPE-FLT-CROTNA-ORIGE-MOVTO
                                            PIC  X(004).
                   07  WAPE-FLT-DENVIO-MOVTO-TARIF
                                            PIC  X(010).
                   07  WAPE-FLT-CNRO-ARQ-MOVTO
                                            PIC  9(002).
                   07  WAPE-FLT-CSEQ-MOVTO  PIC  9(011).
               05  WAPE-PONTEIRO.
                   07  WAPE-PTR-REG-DESPREZA
                                            PIC  9(005).
               05  WAPE-QTDE-OCOR           PIC  9(003).
               05  WAPE-QTDE-TOT-REG        PIC  9(006).
               05  WAPE-FUNC-BDSCO          PIC  X(007).
               05  WAPE-FIM                 PIC  X(001).
           03  FILLER                       PIC  X(080).
