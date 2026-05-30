      *****************************************************************
      * NOME DA INC - I#GFCTOO                                        *
      * DESCRICAO   - COMBO DE TARIFAS COM FLEXIBILIZACAO             *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352 R 208 *
      *****************************************************************
      *
       01  GFCTOO-ENTRADA.
           03  GFCTOO-LL                            PIC S9(004) COMP.
           03  GFCTOO-ZZ                            PIC S9(004) COMP.
           03  GFCTOO-TRANSACAO                     PIC  X(009).
           03  GFCTOO-FUNCAO                        PIC  X(008).
           03  GFCTOO-DADOS-RST.
               05  GFCTOO-FILTRO.
                   07 GFCTOO-CAGPTO-CTA             PIC  9(003).
                   07  GFCTOO-ARGUMENTOS.
ST2506*                09  GFCTOO-ARG1              PIC  9(009).
ST2506*                09  GFCTOO-ARG2              PIC  9(005).
ST2506                 09  GFCTOO-ARG1              PIC  X(009).
ST2506                 09  GFCTOO-ARG2              PIC  X(004).
                       09  GFCTOO-ARG3              PIC  X(002).
                       09  GFCTOO-ARG4              PIC  9(003).
                       09  GFCTOO-ARG5              PIC  X(001).
                       09  GFCTOO-ARG6              PIC  9(005).
                       09  GFCTOO-ARG7              PIC  9(007).
                       09  GFCTOO-ARG8              PIC  9(003).
                       09  GFCTOO-ARG9              PIC  9(003).
               05  GFCTOO-QTDE-OCOR                 PIC  9(007).
               05  GFCTOO-QTDE-TOT-REG              PIC  9(009).
               05  GFCTOO-FUNC-BDSCO                PIC  X(007).
               05  GFCTOO-PONTEIRO.
                   07 GFCTOO-PONTEIRO-TARIFA        PIC  9(005).
               05  GFCTOO-FIM                       PIC  X(001).
               05  GFCTOO-TIPO-VIGENCIA             PIC  X(001).
      *            'V' - VIGENTES
      *            'N' - NAO VIGENTES
      *            'H' - HISTORICO
      *            'T' - TODOS (VIGENTES E NAO VIGENTES)
               05  GFCTOO-TIPO-FUNCAO               PIC  X(001).
      *            'R' - RENOVACAO
      *                SE GFCTOO-CAGPTO-CTA = 04
      *                  VALORIZAR GFCTOO-ARG1, GFCTOO-ARG2, GFCTOO-ARG3
      *                SE GFCTOO-CAGPTO-CTA = 09
      *                  VALORIZAR GFCTOO-ARG4
      *                SE GFCTOO-CAGPTO-CTA = 14
      *                  VALORIZAR GFCTOO-ARG5, GFCTOO-ARG6
      *                SE GFCTOO-CAGPTO-CTA = 15
      *                  VALORIZAR GFCTOO-ARG2, GFCTOO-ARG6
      *                SE GFCTOO-CAGPTO-CTA = 17 OU 20
      *                  VALORIZAR GFCTOO-ARG2, GFCTOO-ARG6
      *                SE GFCTOO-CAGPTO-CTA = 18
      *                  VALORIZAR GFCTOO-ARG7
      *                SE GFCTOO-CAGPTO-CTA = 19
      *                  VALORIZAR GFCTOO-ARG3
      *                SE GFCTOO-CAGPTO-CTA = 26
      *                  VALORIZAR GFCTOO-ARG8,GFCTOO-ARG9
      *            'C' - CANCELAMENTO
      *            'E' - ENCERRAMENTO
      *            'T' - TODAS
      *            'D' - DEFERIDO
      *            'V' - VIGENTE
      *            'P' - PEDIDO
               05  FILLER                           PIC  X(007).
