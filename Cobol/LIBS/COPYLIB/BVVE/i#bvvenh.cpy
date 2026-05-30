      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENH - AREA DO MODULO BVVEA508            *
      *  DESCRICAO.....: OBTER A AGENDA DE RECEBIVEIS DO CLIENTE       *
      *                  MODULO TELEBANCO                              *
      *  TAMANHO.......: 01637 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *                                                                *
      *--RESTART-------------------------------------------------------*
      *                                                                *
      *  PRODUTO (ENVIAR ZEROS NA PRIMEIRA CHAMADA)                    *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  01 - RESTART                                  *
      *                  08 - ERRO CONSISTENCIA                        *
      *                  12 - AGENDA NAO ENCONTRADA                    *
      *                  16 - ERRO DE DB2                              *
      *                                                                *
      *  MSG-RETORNO...: MENSAGEM DE RETORNO                           *
      *  QTDE-OCORR....: QTD OCORRENCIAS RETORNADAS EM CADA CONSULTA   *
      *  DESC-PROD.....: DESCRICAO                                     *
      *  PRODUTO.......: PRODUTO DO RECEBIVEL                          *
      *  VLR-CRED......: VALOR DA AGENDA                               *
      *  VLR-ANTEC.....: VALOR QUE JA FOI ANTECIPADO                   *
      *  VLR-SALDO.....: VALOR DISPONIVEL PARA ANTECIPACAO             *
      *  TOTGER-CREDIT.: VALOR TOTAL CREDITO TODOS PRODUTOS            *
      *  TOTGER-ANTEC..: VALOR TOTAL ANTECIPADO TODOS OS PRODUTOS      *
      *  TOTGER-SALDO..: SALDO GERAL                                   *
      *  NOME-FANTASIA.: NOME FANTASIA                                 *
      *  NOME-RAZAO....: RAZAO SOCIAL                                  *
      *  ENDERECO......: ENDERECO                                      *
      *  CEP-5.........: CEP                                           *
      *  CEP-3.........: COMPLEMENTO CEP                               *
      *  CCGC-CPF......: CNPJ OU CPF                                   *
      *  CFILIAL-CGC...: SE PESSOA FISICA, ZEROS                       *
      *  CCTRL-CPF-CGC.: DIGITO CNPJ/CPF                               *
      *  TIPO-PESSOA...: '00' OU '01'                                  *
      *  BLOQUEIO......: INDICADOR DE CONTA BLOQUEADA NO BISD 'S' 'N'  *
      *  CAMPOS -SIN...: '-' OU '+', RELATIVO AOS CAMPOS DE VALOR      *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENH-COMMAREA.
           05 BVVENH-AREA-ENTRADA.
              10 BVVENH-E-AGENCIA                PIC  9(005).
              10 BVVENH-E-CONTA                  PIC  9(013).

           05 BVVENH-AREA-RESTART.
              10 BVVENH-R-PRODUTO                PIC  9(005).

           05 BVVENH-AREA-SAIDA.
              10 BVVENH-S-COD-RETORNO            PIC  9(002).
              10 BVVENH-S-MSG-RETORNO            PIC  X(050).
              10 BVVENH-S-AGENCIA                PIC  9(005).
              10 BVVENH-S-CONTA                  PIC  9(013).
              10 BVVENH-S-QTDE-OCORR             PIC  9(005).
              10 BVVENH-S-OCORRENCIAS   OCCURS 16 TIMES.
                 15 BVVENH-S-DESC-PROD           PIC  X(025).
                 15 BVVENH-S-PRODUTO             PIC  9(005).
                 15 BVVENH-S-VLR-CRED-SIN        PIC  X(001).
                 15 BVVENH-S-VLR-CRED            PIC  9(015)V9(2).
                 15 BVVENH-S-VLR-ANTEC-SIN       PIC  X(001).
                 15 BVVENH-S-VLR-ANTEC           PIC  9(015)V9(2).
                 15 BVVENH-S-VLR-SALDO-SIN       PIC  X(001).
                 15 BVVENH-S-VLR-SALDO           PIC  9(015)V9(2).
              10 BVVENH-S-TOTGER-CREDITO-SIN     PIC  X(01).
              10 BVVENH-S-TOTGER-CREDITO         PIC  9(15)V99.
              10 BVVENH-S-TOTGER-ANTEC-SIN       PIC  X(01).
              10 BVVENH-S-TOTGER-ANTEC           PIC  9(15)V99.
              10 BVVENH-S-TOTGER-SALDO-SIN       PIC  X(01).
              10 BVVENH-S-TOTGER-SALDO           PIC  9(15)V99.
              10 BVVENH-S-NOME-FANTASIA          PIC  X(35).
              10 BVVENH-S-NOME-RAZAO             PIC  X(40).
              10 BVVENH-S-ENDERECO               PIC  X(40).
              10 BVVENH-S-CEP-5                  PIC  9(05).
              10 BVVENH-S-CEP-3                  PIC  9(03).
              10 BVVENH-S-CCGC-CPF               PIC  9(09).
              10 BVVENH-S-CFILIAL-CGC            PIC  9(04).
              10 BVVENH-S-CCTRL-CPF-CGC          PIC  9(02).
              10 BVVENH-S-TIPO-PESSOA            PIC  X(02).
              10 BVVENH-S-BLOQUEIO               PIC  X(01).
