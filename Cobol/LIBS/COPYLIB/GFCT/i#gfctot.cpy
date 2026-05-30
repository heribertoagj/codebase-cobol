      *****************************************************************
      * NOME DA INC - I#GFCTOT                                        *
      * DESCRICAO   - COMBO DE UF COM FLEXIBILIZACAO                  *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352 R 208 *
      *****************************************************************
      *
       01  GFCTOT-ENTRADA.
           03  GFCTOT-LL                            PIC S9(004) COMP.
           03  GFCTOT-ZZ                            PIC S9(004) COMP.
           03  GFCTOT-TRANSACAO                     PIC  X(009).
           03  GFCTOT-FUNCAO                        PIC  X(008).
           03  GFCTOT-DADOS-RST.
               05  GFCTOT-FILTRO                    PIC  X(002).
               05  GFCTOT-QTDE-OCOR                 PIC  9(007).
               05  GFCTOT-QTDE-TOT-REG              PIC  9(009).
               05  GFCTOT-FUNC-BDSCO                PIC  X(007).
               05  GFCTOT-PONTEIRO.
                   07 GFCTOT-PONTEIRO-UF            PIC  X(002).
               05  GFCTOT-FIM                       PIC  X(001).
               05  GFCTOT-TIPO-VIGENCIA             PIC  X(001).
      *            'V' - VIGENTES
      *            'N' - NAO VIGENTES
      *            'H' - HISTORICO
      *            'T' - TODOS (VIGENTES E NAO VIGENTES)
               05  FILLER                           PIC  X(050).
