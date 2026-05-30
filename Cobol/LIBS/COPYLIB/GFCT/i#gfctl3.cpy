      *****************************************************************
      * NOME DA INC - I#GFCTL3                                        *
      * DESCRICAO   - COMBO DE TARIFAS - PERMISSAO DE ADESAO          *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 15.02.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCTL3-ENTRADA.
           03  GFCTL3-LL                            PIC S9(004) COMP.
           03  GFCTL3-ZZ                            PIC S9(004) COMP.
           03  GFCTL3-TRANSACAO                     PIC  X(009).
           03  GFCTL3-FUNCAO                        PIC  X(008).
           03  GFCTL3-DADOS-RST.
               05  GFCTL3-FILTRO.
                   07 GFCTL3-DATA-REFER             PIC  X(010).
               05  GFCTL3-QTDE-OCOR                 PIC  9(003).
               05  GFCTL3-QTDE-TOT-REG              PIC  9(006).
               05  GFCTL3-FUNC-BDSCO                PIC  X(007).
               05  GFCTL3-PONTEIRO.
                   07 GFCTL3-PONTEIRO-TARIFA        PIC  9(005).
                   07 GFCTL3-PONTEIRO-DATA          PIC  X(010).
               05  GFCTL3-FIM                       PIC  X(001).
               05  GFCTL3-TIPO-VIGENCIA             PIC  X(001).
      *            'T' - TODOS (VIGENTES E NAO VIGENTES)
      *            'V' - VIGENTES
      *            'N' - NAO VIGENTES
      *            'P' - VIGENTES OU FUTURAS
      *            'H' - HISTORICO
      *            'F' - FUTURAS
               05  GFCTL3-TIPO-DATA                 PIC  9(001).
      *            '0' - DATA FORNECIDA PELA INC (GFCTL3-DATA-REFER)
      *            '1' - DATA ANTERIOR PARM_DATA_PROCM
      *            '2' - DATA ATUAL    PARM_DATA_PROCM
      *            '3' - DATA PROXIMA  PARM_DATA_PROCM
               05  FILLER                           PIC  X(035).
