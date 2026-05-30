      *---------------------------------------------------------------*
      * COPYRIGHT..: AGOSTO/2017                                      *
      * NOME.......: BVVEW468                                         *
      * LINGUAGEM..: COBOL                                            *
      * CATEGORIA..: COBOL II CICS                                    *
      * ANALISTA...: HST                                              *
      * PROGRAMADOR: HST                                              *
      * RESUMO ....: CAMADA DE NEGOCIO                                *
      *              IDENTIFICA CODIGO SERIAL DO CERTIFICADO DA CIP   *
      *===============================================================*
      *        H I S T O R I C O   D A S   M A N U T E N C O E S      *
      *---------------------------------------------------------------*
      *   DATA   |  AUTOR    | COMENTARIOS                            *
      *----------+-----------+----------------------------------------*
      * 04/08/17 | ERICA M.  | VERSAO ZERO                            *
      *---------------------------------------------------------------*
       01 LK-7468-AREA-DADOS.
          05 LK-7468-CONTROLE.
             10 LK-7468-CD-RETORNO              PIC 9(003) VALUE ZEROS.
             10 LK-7468-MSG-RETORNO             PIC X(080) VALUE SPACES.
          05 LK-7468-ENTRADA.
             10 LK-7468-CD-AUT-CERT-IN          PIC 9(001) VALUE ZEROS.
             10 LK-7468-CD-SERIE-IN             PIC X(040) VALUE SPACES.
             10 LK-7468-TAM-CHAVE-IN            PIC 9(004) VALUE ZEROS.
             10 LK-7468-NOME-COMUM-IN           PIC X(100) VALUE SPACES.
             10 FILLER                          PIC X(050) VALUE SPACES.
          05 LK-7468-SAIDA.
             10 LK-7468-CD-SERIE-OUT            PIC X(040) VALUE SPACES.
             10 LK-7468-IND-EXISTE-OUT          PIC X(001) VALUE SPACES.
          05 FILLER                             PIC X(050) VALUE SPACES.
