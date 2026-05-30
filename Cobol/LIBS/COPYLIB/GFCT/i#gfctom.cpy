      *****************************************************************
      * NOME DA INC - I#GFCTOM                                        *
      * DESCRICAO   - COMBO DE AGRUPAMENTO COM FLEXIBILIZACAO         *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 01.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352 R 208 *
      *****************************************************************
      *
       01  GFCTOM-ENTRADA.
           03  GFCTOM-LL                            PIC S9(004) COMP.
           03  GFCTOM-ZZ                            PIC S9(004) COMP.
           03  GFCTOM-TRANSACAO                     PIC  X(009).
           03  GFCTOM-FUNCAO                        PIC  X(008).
           03  GFCTOM-DADOS-RST.
               05  GFCTOM-FILTRO                    PIC  9(003).
               05  GFCTOM-QTDE-OCOR                 PIC  9(003).
               05  GFCTOM-QTDE-TOT-REG              PIC  9(006).
               05  GFCTOM-FUNC-BDSCO                PIC  X(007).
               05  GFCTOM-PONTEIRO.
                   07 GFCTOM-PONTEIRO-AGRUP         PIC  9(003).
               05  GFCTOM-FIM                       PIC  X(001).
               05  GFCTOM-TIPO-VIGENCIA             PIC  X(001).
      *            'V' - VIGENTES
      *            'N' - NAO VIGENTES
      *            'H' - HISTORICO
      *            'T' - TODOS (VIGENTES E NAO VIGENTES)
               05  GFCTOM-TIPO-FUNCAO               PIC  X(001).
      *            'R' - RENOVACAO
      *            'C' - CANCELAMENTO
      *            'E' - ENCERRAMENTO
      *            'T' - TODAS
      *            'D' - DEFERIDO
      *            'V' - VIGENTE
      *            'P' - PEDIDO
IN             05  GFCTOM-COD-PERFIL                PIC  X(008).
               05  FILLER                           PIC  X(046).
