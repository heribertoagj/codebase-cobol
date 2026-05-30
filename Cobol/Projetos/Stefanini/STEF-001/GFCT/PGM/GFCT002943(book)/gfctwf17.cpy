      *
      *----------------------------------------------------------------*
      *  AREA DE ENTRADA INVESTIMENTOS ULTIMOS 03 MESES                *
      *  BOOK GFCTWF17                                                 *
      *  LRECL 0200                                                    *
      *----------------------------------------------------------------*
      *  F17-TR            - TIPO REGISTRO(0-HEADER,1-DADOS,9-TRAILER) *
      *                                                                *
      *  F17-TR0              - TIPO REGISTRO  HEADER                  *
      *  F17-DATA-GERACAO     - DATA DE GERACAO AAAAMMDD               *
      *  F17-NOME-ARQUIVO     - NOME ARQUIVO                           *
      *                                                                *
      *  F17-TR1              - TIPO REGISTRO  DADOS                   *
      *  F17-AGENCIA          - AGENCIA HSBC                           *
      *  F17-CONTA            - CONTA HSBC                             *
      *  F17-DIGITO           - DIGITO HSBC                            *
      *  F17-CPF-CNPJ         -                                        *
      *  F17-FILIAL           -                                        *
      *  F17-CESTA            - CESTA HSBC                             *
      *  F17-VSDO-MED-FUNDO   - SALDO MEDIO FUNDOS                     *
      *  F17-VSDO-MED-POUP    - SALDO MEDIO POUPANCA                   *
      *  F17-VSDO-MED-CDB     - SALDO MEDIO CDB                        *
      *  F17-VSDO-MED-PREVD   - SALDO MEDIO PREVIDENCIA                *
      *  F17-VSDO-APLIC-ACOES - SALDO ULTIMO DIA DO MES EM ACOES       *
      *  F17-VSDO-APLIC-TSORO - SALDO ULT.DIA DO MES EM TESOURO DIRETO *
      *                                                                *
      *  F17-TR9           - TIPO REGISTRO  TRAILER                    *
      *  F17-QTDE-REGISTROS                                            *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  F17-REGISTRO.
           05 FILLER                       PIC  X(199).

       01  F17-TR0  REDEFINES F17-REGISTRO.
           05 FILLER                       PIC  9(001).
           05 F17-DATA-GERACAO             PIC  9(008).
           05 F17-NOME-ARQUIVO             PIC  X(030).
           05 FILLER                       PIC  X(161).

       01  F17-TR1  REDEFINES F17-REGISTRO.
           05 FILLER                       PIC  9(001).
           05 F17-DADOS.
              10 F17-AGENCIA               PIC  9(005).
              10 F17-CONTA                 PIC  9(013).
              10 F17-DIGITO                PIC  9(001).
ST2507*       10 F17-CPF-CNPJ              PIC  9(009).
ST2507*       10 F17-FILIAL                PIC  9(004).
ST2507        10 F17-CPF-CNPJ              PIC  X(009).
ST2507        10 F17-FILIAL                PIC  X(004).
              10 F17-DIGITO-FJ             PIC  9(002).
              10 F17-AM-APUR               PIC  9(006).
              10 F17-VSDO-MED-FUNDO        PIC  9(015)V99.
              10 F17-VSDO-MED-POUP         PIC  9(015)V99.
              10 F17-VSDO-MED-CDB          PIC  9(015)V99.
              10 F17-VSDO-MED-PREVD        PIC  9(015)V99.
              10 F17-VSDO-APLIC-ACOES      PIC  9(015)V99.
              10 F17-VSDO-APLIC-TSORO      PIC  9(015)V99.
           05 FILLER                       PIC  X(052).

       01  F17-TR9  REDEFINES F17-REGISTRO.
           05 FILLER                       PIC  9(001).
           05 F17-QTDE-REGISTROS           PIC  9(015).
           05 FILLER                       PIC  X(184).
      *
      *------------------ FIM DA BOOK GFCTWF09 ------------------------*
      *
