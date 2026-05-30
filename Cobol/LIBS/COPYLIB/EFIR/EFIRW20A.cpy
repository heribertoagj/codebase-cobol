000100******************************************************************
000200* SISTEMA : EFIR - EFIRW20A                                      *
000300* TAMANHO : 0270 BYTES                                           *
000400* ARQUIVO : PROCESSO DE RECEPCAO DE MOVIMENTO                    *
000500* INCONSISTENCIA ENCONTRADAS NA RECEPCAO DO MOVIMENTO            *
000500******************************************************************
001000* EFIRW20A-CNPJ-DECLA....... DECLARANTE
001000* EFIRW20A-NOME-DECLA....... NOME DO DECLARANTE
001000* EFIRW20A-PAPEL............ 1-PATROCINADO   -> 01, 10
001000*                            2-INTERMEDIARIO -> 02, 20
001000*                            3-DECLARADO     -> 05, 50
001000*                            4-PROPRIETARIO  -> 51
001000* EFIRW20A-NI............... DOCUMENTO DO PERSONAGEM
      * EFIRW20A-CCTA-ENVIO-INFME. NRO CONTA -> 52,53,54
      * EFIRW20A-DATA-MOVTO....... DATA DO MOVIMENTO DA CONTA -> 53,54
001000* EFIRW20A-CODERRO.......... CODIGO DO ERRO
001000* EFIRW20A-MENSAG........... MENSAGEM
001000* EFIRW20A-TPREG............ TIPO DE REGISTRO
000600******************************************************************
000700 05  EFIRW20A.
001000     10 EFIRW20A-CNPJ-DECLA       PIC X(014).
001000     10 EFIRW20A-NOME-DECLA       PIC X(060).
001000     10 EFIRW20A-PAPEL            PIC 9(001).
001000     10 EFIRW20A-NI               PIC X(025).
           10 EFIRW20A-CCTA-ENVIO-INFME PIC X(050)     VALUE SPACES.
           10 EFIRW20A-DATA-MOVTO       PIC X(010)     VALUE SPACES.
001000     10 EFIRW20A-CODERRO          PIC X(008).
001000     10 EFIRW20A-MENSAG           PIC X(080).
001000     10 EFIRW20A-TPREG            PIC 9(002).
001000     10 FILLER                    PIC X(020).
000600******************************************************************
