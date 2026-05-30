      *****************************************************************
      * NOME DA INC - I#GFCTOR                                        *
      * DESCRICAO   - COMBO DE SEGMENTO COM FLEXIBILIZACAO            *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352 R 208 *
      *****************************************************************
      *
       01  GFCTOR-ENTRADA.
           03  GFCTOR-LL                            PIC S9(004) COMP.
           03  GFCTOR-ZZ                            PIC S9(004) COMP.
           03  GFCTOR-TRANSACAO                     PIC  X(009).
           03  GFCTOR-FUNCAO                        PIC  X(008).
           03  GFCTOR-DADOS-RST.
               05  GFCTOR-FILTRO                    PIC  9(003).
               05  GFCTOR-QTDE-OCOR                 PIC  9(007).
               05  GFCTOR-QTDE-TOT-REG              PIC  9(009).
               05  GFCTOR-FUNC-BDSCO                PIC  X(007).
               05  GFCTOR-PONTEIRO.
                   07 GFCTOR-PONTEIRO-SEGTO         PIC  9(003).
               05  GFCTOR-FIM                       PIC  X(001).
               05  GFCTOR-TIPO-VIGENCIA             PIC  X(001).
      *            'V' - VIGENTES
      *            'N' - NAO VIGENTES
      *            'H' - HISTORICO
      *            'T' - TODOS (VIGENTES E NAO VIGENTES)
               05  FILLER                           PIC  X(048).
