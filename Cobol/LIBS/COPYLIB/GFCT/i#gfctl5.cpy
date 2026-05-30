      ******************************************************************
      * NOME DA INC - I#GFCTL5                                         *
      * DESCRICAO   - COMBO DE AGRUPAMENTO - PERMISSAO DE ADESAO       *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 15/02/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTL5-ENTRADA.
           03  GFCTL5-LL                            PIC S9(004) COMP.
           03  GFCTL5-ZZ                            PIC S9(004) COMP.
           03  GFCTL5-TRANSACAO                     PIC  X(009).
           03  GFCTL5-FUNCAO                        PIC  X(008).
           03  GFCTL5-DADOS-RST.
               05  GFCTL5-FILTRO.
                   07 GFCTL5-CSERVC-TARIF           PIC  9(005).
                   07 GFCTL5-DATA-REFER             PIC  X(010).
               05  GFCTL5-QTDE-OCOR                 PIC  9(003).
               05  GFCTL5-QTDE-TOT-REG              PIC  9(006).
               05  GFCTL5-FUNC-BDSCO                PIC  X(007).
               05  GFCTL5-PONTEIRO.
                   07 GFCTL5-PONTEIRO-AGRUP         PIC  9(003).
                   07 GFCTL5-PONTEIRO-DATA          PIC  X(010).
               05  GFCTL5-FIM                       PIC  X(001).
               05  GFCTL5-TIPO-VIGENCIA             PIC  X(001).
      *            'T' - TODOS (VIGENTES E NAO VIGENTES)
      *            'V' - VIGENTES
      *            'N' - NAO VIGENTES
      *            'P' - VIGENTES OU FUTURAS
      *            'H' - HISTORICO
      *            'F' - FUTURAS
               05  GFCTL5-TIPO-DATA                 PIC  9(001).
      *            '0' - DATA FORNECIDA PELA INC (GFCTL5-DATA-REFER)
      *            '1' - DATA ANTERIOR PARM_DATA_PROCM
      *            '2' - DATA ATUAL    PARM_DATA_PROCM
      *            '3' - DATA PROXIMA  PARM_DATA_PROCM
               05  FILLER                           PIC  X(032).
