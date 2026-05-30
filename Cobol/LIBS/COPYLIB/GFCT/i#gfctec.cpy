      *****************************************************************
      * NOME DA INC - I#GFCTEC                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - TARIFAS                      *
      *               COMBO DE HISTORICO DE TARIFAS                   *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - JULHO/2005                                      *
      * RESPONSAVEL - RENATO TAMANAHA - PROCWORK - TEL 41524352       *
      *****************************************************************
      *
       01  GFCTEC-ENTRADA.
           03  GFCTEC-LL                            PIC S9(004) COMP.
           03  GFCTEC-ZZ                            PIC S9(004) COMP.
           03  GFCTEC-TRANSACAO                     PIC  X(009).
           03  GFCTEC-FUNCAO                        PIC  X(008).
           03  GFCTEC-DADOS-RST.
               05  GFCTEC-FILTRO.
                   07 GFCTEC-CSERVC-TARIF           PIC  9(005).
               05  GFCTEC-QTDE-OCOR                 PIC  9(003).
               05  GFCTEC-QTDE-TOT-REG              PIC  9(006).
               05  GFCTEC-FUNC-BDSCO                PIC  X(007).
               05  GFCTEC-TERMINAL                  PIC  X(008).
               05  GFCTEC-PONTEIRO                  PIC  9(005).
               05  GFCTEC-FIM                       PIC  X(001).
               05  GFCTEC-CSIT-LIBRC-TARIF          PIC  9(002).
      *            '01' - DEFERIDAS                (7)
      *            '02' - STATUS               >= 5  E  <> 9
      *            '03' - INDEFERIDAS              (8)
      *            '04' - AGUARDAR PARECER         (6)
      *            '05' - CANCELADO NA ORIGEM      (9)
      *            '06' - RETORNADO AO SOLICITANTE (10)
      *            '07' - TODOS STATUS
      *            '08' - INCLUS O CONCLUÍDA       (5)
               05  GFCTEC-ID-MASTER                 PIC  X(001).
               05  FILLER                           PIC  X(041).
