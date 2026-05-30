      ******************************************************************
      *  RESOLUCAO 4734 - CLIENTES MASSIVOS                            *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEFX - AREA DO MODULO BVVEA520          *
      *  DESCRICAO.......: IDENTIFICAR CLIENTES MASSIVOS               *
      *  TAMANHO.........: 182 BYTES                                   *
      *--SAIDA---------------------------------------------------------*
      *  COD-RETORNO.....: 00 - PROCESSAMENTO OK                       *
      *                    08 - ERRO CONSISTENCIA                      *
      *                    16 - ERRO DE DB2                            *
      *                                                                *
      *  IND-CLI-MASSV...: 'S' SIM OU 'N' NAO                          *
      ******************************************************************
      *                                                                *
       01  BVVEFX-COMMAREA.
           05 BVVEFX-AREA-ENTRADA.
              10 BVVEFX-E-AGENCIA                      PIC  9(005).
              10 BVVEFX-E-CONTA                        PIC  9(013).

           05 BVVEFX-AREA-SAIDA.
              10 BVVEFX-S-COD-RETORNO                  PIC  9(002).
              10 BVVEFX-S-MSG-RETORNO                  PIC  X(050).
              10 BVVEFX-S-IND-CLI-MASSV                PIC  X(001).
