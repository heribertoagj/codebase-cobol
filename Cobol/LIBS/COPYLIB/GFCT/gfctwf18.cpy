      *
      *----------------------------------------------------------------*
      *  AREA DE REGEITADOS INVESTIMENTOS ULTIMOS 03 MESES             *
      *  BOOK GFCTWF18                                                 *
      *  LRECL 0200                                                    *
      *----------------------------------------------------------------*
      *  F18-DADOS            - TIPO REGISTRO  DADOS                   *
      *  F18-AGENCIA          - AGENCIA HSBC                           *
      *  F18-CONTA            - CONTA HSBC                             *
      *  F18-DIGITO           - DIGITO HSBC                            *
      *  F18-CPF-CNPJ         -                                        *
      *  F18-FILIAL           -                                        *
      *  F18-DIGITO-FJ        -                                        *
      *  F18-CESTA            - CESTA HSBC                             *
      *  F18-VSDO-MED-FUNDO   - SALDO MEDIO FUNDOS                     *
      *  F18-VSDO-MED-POUP    - SALDO MEDIO POUPANCA                   *
      *  F18-VSDO-MED-CDB     - SALDO MEDIO CDB                        *
      *  F18-VSDO-MED-PREVD   - SALDO MEDIO PREVIDENCIA                *
      *  F18-VSDO-APLIC-TSORO - SALDO ULT.DIA DO MES EM TESOURO DIRETO *
      *  F18-MSG-ERRO         - MENSAGEM DE ERRO                       *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  F18-REGISTRO.
           05 F18-DADOS.
              10 F18-AGENCIA               PIC  9(005).
              10 F18-CONTA                 PIC  9(013).
              10 F18-DIGITO                PIC  9(001).
ST2507*       10 F18-CPF-CNPJ              PIC  9(009).
ST2507*       10 F18-FILIAL                PIC  9(004).
ST2507        10 F18-CPF-CNPJ              PIC  X(009).
ST2507        10 F18-FILIAL                PIC  X(004).
              10 F18-CESTA                 PIC  9(005).
              10 F18-AM-APUR               PIC  9(006).
              10 F18-VSDO-MED-FUNDO        PIC  9(015)V99.
              10 F18-VSDO-MED-POUP         PIC  9(015)V99.
              10 F18-VSDO-MED-CDB          PIC  9(015)V99.
              10 F18-VSDO-MED-PREVD        PIC  9(015)V99.
              10 F18-VSDO-APLIC-ACOES      PIC  9(015)V99.
              10 F18-VSDO-APLIC-TSORO      PIC  9(015)V99.
           05 F18-MSG-ERRO                 PIC  X(030).
           05 F18-RESERVA                  PIC  X(023).
