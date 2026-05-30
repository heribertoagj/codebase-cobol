      *****************************************************************
      * NOME DA INC - I#GFCTKP                                        *
      * DESCRICAO   - COMBO DE TARIFAS GERENCIADAS                    *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 07.06.2005                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCTKP-ENTRADA.
           03  GFCTKP-LL                            PIC S9(004) COMP.
           03  GFCTKP-ZZ                            PIC S9(004) COMP.
           03  GFCTKP-TRANSACAO                     PIC  X(009).
           03  GFCTKP-FUNCAO                        PIC  X(008).
           03  GFCTKP-DADOS-RST.
               05  GFCTKP-FILTRO.
                   07 GFCTKP-CSERVC-TARIF           PIC  9(005).
                   07 GFCTKP-DATA-REFER             PIC  X(010).
               05  GFCTKP-QTDE-OCOR                 PIC  9(003).
               05  GFCTKP-QTDE-TOT-REG              PIC  9(006).
               05  GFCTKP-FUNC-BDSCO                PIC  X(007).
               05  GFCTKP-TERMINAL                  PIC  X(008).
               05  GFCTKP-PONTEIRO.
                   07 GFCTKP-PONTEIRO-TARIFA        PIC  9(005).
                   07 GFCTKP-PONTEIRO-DATA          PIC  X(010).
               05  GFCTKP-FIM                       PIC  X(001).
               05  GFCTKP-TIPO-VIGENCIA             PIC  X(001).
      *            'T' - TODOS (VIGENTES E NAO VIGENTES)
      *            'V' - VIGENTES
      *            'N' - NAO VIGENTES
      *            'P' - VIGENTES OU FUTURAS
      *            'H' - HISTORICO
      *            'F' - FUTURAS
               05  GFCTKP-TIPO-DATA                 PIC  9(001).
      *            '0' - DATA FORNECIDA PELA INC (GFCTKP-DATA-REFER)
      *            '1' - DATA ANTERIOR PARM_DATA_PROCM
      *            '2' - DATA ATUAL    PARM_DATA_PROCM
      *            '3' - DATA PROXIMA  PARM_DATA_PROCM
               05  FILLER                           PIC  X(022).
