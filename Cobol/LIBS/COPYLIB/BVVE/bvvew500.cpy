      ******************************************************************
      * NOME BOOK : BVVEW500 - HEADER DE ARQUIVOS ARRC                 *
      *                                                                *
      * DESCRICAO : HEADER DE ARQUIVO ARRC DE OPERACOES RECEBIVEIS     *
      *             EM LOTE                                            *
      * DATA      : AGOSTO/2020                                        *
      * AUTOR     : MAXIMILIANO BARBOSA  -  7COMM INFORMATICA          *
      * TAMANHO   : 2956 BYTES                                         *
      * ---------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      * ---------------------------------------------------------------*
      * BVVEW500-NOME-ARQUIVO        : NOME ARQUIVO COM PREFIXO "ARRC" *
      * BVVEW500-NRO-CTRL-IF         : NRO CONTROLE INST. FINCR.       *
      * BVVEW500-NRO-CTRL-ORIG       : NRO CONTROLE ORIGEM             *
      * BVVEW500-ISPB-EMISSOR        : ISPB EMISSOR                    *
      * BVVEW500-ISPB-DESTINO        : ISPB DESTINO                    *
      * BVVEW500-DT-HR               : TIMESTAMP GERACAO ARQUIVO       *
      * BVVEW500-SIT-REQ             : SITUACAO REQUISICAO             *
      * BVVEW500-IND-CONTIN          : INDICADOR DE CONTINGENCIA       *
      * BVVEW500-DATA-MOVIM          : DATA MOVIMENTO                  *
      ******************************************************************
       01  BVVEW500-ARRC-HEADER.
           05  BVVEW500-NOME-ARQUIVO   PIC  X(050).
           05  BVVEW500-NRO-CTRL-IF    PIC  X(020).
           05  BVVEW500-NRO-CTRL-ORIG  PIC  X(020).
           05  BVVEW500-ISPB-EMISSOR   PIC  X(008).
           05  BVVEW500-ISPB-DESTINO   PIC  X(008).
           05  BVVEW500-DT-HR          PIC  X(026).
           05  BVVEW500-SIT-REQ        PIC  X(002).
           05  BVVEW500-IND-CONTIN     PIC  X(001).
           05  BVVEW500-DATA-MOVIM     PIC  X(008).
