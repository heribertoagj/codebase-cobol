      ******************************************************************
      * NOME BOOK : BVVEWODL                                           *
      * DESCRICAO : BOOK DE INTERFACE ENTRE PGM COORDENADOR E FUNCIONAL*
      * DATA      : 06/07/2023                                         *
      * AUTOR     : CLERISTON MARZENDA DE SOUZA                        *
      * EMPRESA   : FOURSYS                                            *
      * TAMANHO   : 1199 BYTES                                         *
      ************************** DESCRICAO *****************************
      *                                                                *
      * BVVEWODL-E-TPO-OPER...: CODIGO DA OPERACAO 01 DATA OK, 00 VAZIO*
      * BVVEWODL-E-DATA-INICIO: DATA INICIO                            *
      * BVVEWODL-E-DATA-FIM...: DATA FIM                               *
      * BVVEWODL-E-PARM-31....: QUANTIDADE DIAS LIMITE PRARA PERIODO   *
      * BVVEWODL-E-PARM-32....: QUANTIDADE DE DIAS PARA DATA INICIO    *
      * BVVEWODL-E-PARM-33....: QUANTIDADE DE DIAS TOTAL PARA O PERIODO*
      *                                                                *
      ******************************************************************
           05 BVVEWODL-HEADER.
             10 BVVEWODL-COD-LAYOUT    PIC X(008)  VALUE 'BVVEWODL'.
             10 BVVEWODL-TAM-LAYOUT    PIC 9(005)  VALUE 1083.
           05 BVVEWODL-REGISTRO.
             10 BVVEWODL-BLOCO-ENTRADA.
               15 BVVEWODL-E-TPO-OPER         PIC 9(001) VALUE ZEROS.
               15 BVVEWODL-E-DATA-INICIO      PIC X(010) VALUE SPACES.
               15 BVVEWODL-E-DATA-FIM         PIC X(010) VALUE SPACES.
               15 BVVEWODL-E-PARAMETROS.
                  20 BVVEWODL-E-PARM-31       PIC 9(005) VALUE ZEROS.
                  20 BVVEWODL-E-PARM-32       PIC 9(005) VALUE ZEROS.
                  20 BVVEWODL-E-PARM-33       PIC 9(005) VALUE ZEROS.
                  20 FILLER                   PIC X(025).
             10 BVVEWODL-BLOCO-SAIDA.
               15 BVVEWODL-S-NUM-OCORRENCIAS  PIC 9(005) VALUE ZEROS.
               15 BVVEWODL-S-DATA-INI-RANGE   PIC X(010) VALUE SPACES.
               15 BVVEWODL-S-DATA-FIM-RANGE   PIC X(010) VALUE SPACES.
               15 BVVEWODL-S-DADOS-SAIDA  OCCURS 50 TIMES.
                 20 BVVEWODL-S-DATA-INI       PIC  X(010) VALUE SPACES.
                 20 BVVEWODL-S-DATA-FIM       PIC  X(010) VALUE SPACES.
