      *****************************************************************
      * NOME DA INC - I#GFCT7L                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - TARIFAS                      *
      *               COMBO DE HISTORICO DE TARIFAS                   *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 23.03.2005                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCT7L-ENTRADA.
           03  GFCT7L-LL                            PIC S9(004) COMP.
           03  GFCT7L-ZZ                            PIC S9(004) COMP.
           03  GFCT7L-TRANSACAO                     PIC  X(009).
           03  GFCT7L-FUNCAO                        PIC  X(008).
           03  GFCT7L-DADOS-RST.
               05  GFCT7L-FILTRO.
                   07 GFCT7L-CSERVC-TARIF           PIC  9(005).
               05  GFCT7L-QTDE-OCOR                 PIC  9(003).
               05  GFCT7L-QTDE-TOT-REG              PIC  9(006).
               05  GFCT7L-FUNC-BDSCO                PIC  X(007).
               05  GFCT7L-TERMINAL                  PIC  X(008).
               05  GFCT7L-PONTEIRO                  PIC  9(005).
               05  GFCT7L-FIM                       PIC  X(001).
               05  GFCT7L-CSIT-LIBRC-TARIF          PIC  9(002).
      *            PARM | DESCRICAO                 |DOMINIO
      *            --------------------------------------------
      *            '01' | DEFERIDO                  | 7
      *            '02' | DEFERIDO E AGUARDANDO     |
      *                 | PARECER E CONCLUIDO       |
      *                 | INCLUSAO E RETORNADO AO   |
      *                 | SOLICITANTE               | >=5 E <> 9
      *            '03' | INDEFERIDO                | 8
      *            '04' | AGUARDAR PARECER          | 6
      *            '05' | CANCELADO NA ORIGEM       | 9
      *            '06' | RETORNADO AO SOLICITANTE  | 10
      *            '07' | TODOS DOMINIOS A PARTIR   |
      *                 | DE CONCLUIDA INCLUSAO     | >=5
      *            '08' | CONCLUIDA INCLUSAO        | 5
      *            '09' | DEFERIDO E AGUARDANDO     |
      *                 | PARECER                   | >=5 E <=7
      *            '10' | DEFERIDO PASSADO          | 11
      *            ---------------------------------------------
               05  GFCT7L-PONTEIRO-SEQ              PIC  9(002).
               05  FILLER                           PIC  X(040).
