      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      * NOME DA INC - GFCTWAIC                                         *
      * DESCRICAO   - EXTRATO ANUAL DE TARIFAS - TAUTRZ_ENVIO_CLI      *
      *               ESPELHO DA TABELA GFCTB0L5 - CLIENTE             *
      * DATA        - JULHO/2009                                       *
      * RESPONSAVEL - VINICIUS CRISTIAN MADUREIRA                      *
ST2507* LRECL       - 044                                              *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  REG-WAIC.
           03 WAIC-CAGPTO-CTA                PIC S9(003)V USAGE COMP-3.
           03 WAIC-CINDCD-AUTRZ-EMIS         PIC S9(001)V USAGE COMP-3.
           03 WAIC-HINCL-REG-SIST            PIC  X(026).
ST2507*    03 WAIC-CCNPJ-CPF                 PIC S9(009)V USAGE COMP-3.
ST2507*    03 WAIC-CFLIAL-CGC                PIC S9(005)V USAGE COMP-3.
ST2507     03 WAIC-CCNPJ-CPF                 PIC  X(009).
ST2507     03 WAIC-CFLIAL-CGC                PIC  X(004).
           03 WAIC-CCTRL-CPF-CGC             PIC S9(002)V USAGE COMP-3.

