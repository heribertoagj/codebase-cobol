      *----------------------------------------------------------------*
      *            Book de mapeamento da comunicacao com a cip         *
      *----------------------------------------------------------------*
      *
       01          BVVEWMAP-AREA.
            05     BVVEWMAP-E01-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E01                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,GET     '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,NB     '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,NB     '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWR01'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E01                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E01                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E01                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E02-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E02                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,GET     '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,NB     '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,NB     '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,STRING   '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWE10'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,S'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E02                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E02                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E02                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E03-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E03                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,POST    '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,JSON   '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,JSON   '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWR11'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E03                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E03                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E03                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E04-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E04                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,DELETE  '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,NB     '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,NB     '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWR13'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E04                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E04                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E04                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E05-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E05                PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,POST    '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,JSON   '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,JSON   '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWR27'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E05                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E05                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E05                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E06-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E06                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,PUT     '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,JSON   '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,JSON   '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWR00'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E06                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E06                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E06                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E07-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E07                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,POST    '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,NA     '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,NA     '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWR19'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E07                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E07                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E07                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E08-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E08                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,POST    '.
              10 FILLER                      PIC X(026) VALUE
                 ';envio_entrada,ENCAPSULADO'.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,JSON   '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWR00'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E08                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E08                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E08                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E09-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E09                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,POST    '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,JSON   '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,JSON   '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWI19'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,A'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(011) VALUE
                 ';call_back,'.
              10 WRK-URL-CB-09               PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E09                 PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E09                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E09                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E10-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E10                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,POST    '.
              10 FILLER                      PIC X(025) VALUE
                 ';envio_entrada,STRING    '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,JSON   '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWI19'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,A'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(011) VALUE
                 ';call_back,'.
              10 WRK-URL-CB-10               PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E10                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E10                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E10                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E11-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E11                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,GET     '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,NB     '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,NB     '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWR21'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,S'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E11                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E11                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E11                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E12-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E12                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,PUT     '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,JSON   '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,JSON   '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWI19'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,A'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(011) VALUE
                 ';call_back,'.
              10 WRK-URL-CB-12               PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E12                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E12                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E12                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E13-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E13                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,PUT     '.
              10 FILLER                      PIC X(025) VALUE
                 ';envio_entrada,STRING    '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,JSON   '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWI19'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,A'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(011) VALUE
                 ';call_back,'.
              10 WRK-URL-CB-13               PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E13                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E13                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E13                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E14-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E14                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,GET     '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,NB     '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,NB     '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWR21'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E14                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E14                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E14                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E15-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E15                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,PATCH   '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,JSON   '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,JSON   '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWR20'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,A'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(011) VALUE
                 ';call_back,'.
              10 WRK-URL-CB-15               PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E15                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E15                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E15                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E16-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E16                PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,GET     '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,NB     '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,NB     '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWO21'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,S'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E16                 PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E16                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E16                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E17-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E17                PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,GET     '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,NB     '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,NB     '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWA21'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,S'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E17                 PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E17                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E17                  PIC X(032) VALUE SPACES.
      *
            05     BVVEWMAP-E18-HEADER.
              10 FILLER                      PIC X(012) VALUE
                 'destino_url,'.
              10 WRK-URL-E18                 PIC X(040) VALUE SPACES.
              10 FILLER                      PIC X(016) VALUE
                 ';metodo,POST    '.
              10 FILLER                      PIC X(022) VALUE
                 ';envio_entrada,JSON   '.
              10 FILLER                      PIC X(020) VALUE
                 ';envio_saida,JSON   '.
              10 FILLER                      PIC X(026) VALUE
                 ';retorno_entrada,JSON     '.
              10 FILLER                      PIC X(024) VALUE
                 ';retorno_saida,JSON     '.
              10 FILLER                      PIC X(016) VALUE
                 ';layout,BVVEWR00'.
              10 FILLER                      PIC X(007) VALUE
                 ';tipo,S'.
              10 FILLER                      PIC X(028) VALUE
                 ';pagina_automatica_retorno,N'.
              10 FILLER                      PIC X(015) VALUE
                 ';call_back,NULL'.
              10 FILLER                      PIC X(004) VALUE
                 ';id,'.
              10 WRK-ID-E18                  PIC X(020) VALUE SPACES.
              10 FILLER                      PIC X(013) VALUE
                 ';querystring,'.
              10 WRK-QS-E18                  PIC X(256) VALUE SPACES.
              10 FILLER                      PIC X(017) VALUE
                 ';kid_certificado,'.
              10 WRK-SE-E18                  PIC X(032) VALUE SPACES.
      *
           05  BVVEWMAP-E19-BODY.
             10 FILLER     PIC X(033)  VALUE
             'idendPartOrigdrContstc,string,14;'.
             10 FILLER     PIC X(027)  VALUE
             'idendPartContstd,string,14;'.
             10 FILLER     PIC X(031)  VALUE
             'identdCtrlReqSolicte,string,20;'.
             10 FILLER     PIC X(025)  VALUE
             'indrMotvContstc,string,2;'.
             10 FILLER     PIC X(024)  VALUE
             'descContstc,string,0200;'.
             10 FILLER     PIC X(026)  VALUE
             'identdCtrlOptIn,string,19;'.
      *
           05  BVVEWMAP-E01-BODY.
             10 FILLER     PIC X(014)  VALUE
             'msg,string,50;'.
      *
           05  BVVEWMAP-E03-BODY.
             10 FILLER     PIC X(031)  VALUE
             'identdCtrlReqSolicte,string,20;'.
             10 FILLER     PIC X(052)  VALUE
             'cnpjOuCnpjBaseOuCpfUsuFinalRecbdrOuTitlar,string,14;'.
             10 FILLER     PIC X(024)  VALUE
             'cnpjCreddrSub,string,14;'.
             10 FILLER     PIC X(021)  VALUE
             'cnpjFincdr,string,14;'.
             10 FILLER     PIC X(030)  VALUE
             'codInstitdrArrajPgto,string,3;'.
             10 FILLER     PIC X(018)  VALUE
             'dtOptIn,string,10;'.
             10 FILLER     PIC X(021)  VALUE
             'dtIniOptIn,string,10;'.
             10 FILLER     PIC X(021)  VALUE
             'dtFimOptIn,string,10;'.
             10 FILLER     PIC X(019)  VALUE
             'indrDomcl,string,1;'.
      *
           05  BVVEWMAP-E05-BODY.
             10 FILLER     PIC X(033)  VALUE
             'idendPartOrigdrContstc,string,14;'.
             10 FILLER     PIC X(027)  VALUE
             'idendPartContstd,string,14;'.
             10 FILLER     PIC X(031)  VALUE
             'identdCtrlReqSolicte,string,20;'.
             10 FILLER     PIC X(025)  VALUE
             'indrMotvContstc,string,2;'.
             10 FILLER     PIC X(024)  VALUE
             'descContstc,string,0200;'.
*************10 FILLER     PIC X(026)  VALUE
*************'identdCtrlOptIn,string,19;'.
             10 FILLER     PIC X(019)  VALUE
             'identdOp,string,19;'.
      *
           05  BVVEWMAP-E06-BODY.
             10 FILLER     PIC X(033)  VALUE
             'idendPartOrigdrContstc,string,14;'.
             10 FILLER     PIC X(027)  VALUE
             'idendPartContstd,string,14;'.
             10 FILLER     PIC X(031)  VALUE
             'identdCtrlReqSolicte,string,20;'.
             10 FILLER     PIC X(025)  VALUE
             'indrMotvContstc,string,2;'.
             10 FILLER     PIC X(024)  VALUE
             'descContstc,string,0200;'.
*************10 FILLER     PIC X(026)  VALUE
*************'identdCtrlOptIn,string,19;'.
             10 FILLER     PIC X(019)  VALUE
             'identdOp,string,19;'.
      *
           05  BVVEWMAP-E08-BODY.
             10 FILLER     PIC X(030)  VALUE
*************'dados,array,20,12;Ý;,object,0;'.
             'dados,array,50,12;Ý;,object,0;'.
             10 FILLER     PIC X(036)  VALUE
             'cnpjOuCnpjBaseOuCpfTitlar,string,14;'.
             10 FILLER     PIC X(028)  VALUE
             'cnpjOuCpfTitlarCt,string,14;'.
             10 FILLER     PIC X(023)  VALUE
             'ispbBcoRecbdr,string,8;'.
             10 FILLER     PIC X(014)  VALUE
             'tpCt,string,2;'.
             10 FILLER     PIC X(009)  VALUE
             'ag,int,4;'.
             10 FILLER     PIC X(010)  VALUE
             'ct,int,13;'.
*************10-FILLER     PIC-X(014) -VALUE
************-'ctPgto,int,20;'.
             10 FILLER     PIC X(024)  VALUE
             'cnpjCreddrSub,string,14;'.
             10 FILLER     PIC X(034)  VALUE
             'cnpjOuCpfUsuFinalRecbdr,string,14;'.
             10 FILLER     PIC X(026)  VALUE
             'cnpjOuCpfTitlar,string,14;'.
             10 FILLER     PIC X(030)  VALUE
             'codInstitdrArrajPgto,string,3;'.
             10 FILLER     PIC X(024)  VALUE
             'dtPrevtLiquid,string,10;'.
             10 FILLER     PIC X(029)  VALUE
             'vlrOuPercNegcd,int,14,,dec,2;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
      *
           05  BVVEWMAP-E09-BODY.
             10 FILLER     PIC X(015)  VALUE
             'tpObj,string,1;'.
             10 FILLER     PIC X(027)  VALUE
             'identdNegcRecbvl,string,40;'.
             10 FILLER     PIC X(020)  VALUE
             'indrTpNegc,string,2;'.
             10 FILLER     PIC X(019)  VALUE
             'dtVencOp,string,10;'.
             10 FILLER     PIC X(034)  VALUE
             'vlrTotLimOuSldDevdr,int,14,,dec,2;'.
             10 FILLER     PIC X(020)  VALUE
             'indrGestER,string,1;'.
             10 FILLER     PIC X(022)  VALUE
             'indrRegrDivs,string,1;'.
             10 FILLER     PIC X(027)  VALUE
             'indrActeIncondlOp,string,1;'.
             10 FILLER     PIC X(032)  VALUE
             'identdConjUniddRecbvl,string,19;'.
TM           10 FILLER     PIC X(036)  VALUE
.            'indrAlcancContrtoCreddrSub,string,1;'.
.            10 FILLER     PIC X(035)  VALUE
.            'indrActeUniddRecbvlReserv,string,1;'.
      *

           05  BVVEWMAP-E12-BODY.
             10 FILLER     PIC X(015)  VALUE
             'tpObj,string,1;'.
             10 FILLER     PIC X(027)  VALUE
             'identdNegcRecbvl,string,40;'.
             10 FILLER     PIC X(020)  VALUE
             'indrTpNegc,string,2;'.
             10 FILLER     PIC X(019)  VALUE
             'dtVencOp,string,10;'.
             10 FILLER     PIC X(034)  VALUE
             'vlrTotLimOuSldDevdr,int,14,,dec,2;'.
             10 FILLER     PIC X(020)  VALUE
             'indrGestER,string,1;'.
             10 FILLER     PIC X(022)  VALUE
             'indrRegrDivs,string,1;'.
             10 FILLER     PIC X(036)  VALUE
             'indrAlcancContrtoCreddrSub,string,1;'.
             10 FILLER     PIC X(027)  VALUE
             'indrActeIncondlOp,string,1;'.
             10 FILLER     PIC X(035)  VALUE
             'indrActeUniddRecbvlReserv,string,1;'.
             10 FILLER     PIC X(036)  VALUE
             'identdOpDescstcNegcRecbvl,string,19;'.
             10 FILLER     PIC X(032)  VALUE
             'identdConjUniddRecbvl,string,19;'.
             10 FILLER     PIC X(022)  VALUE
             'indrAutcCess,string,1;'.
             10 FILLER     PIC X(048)  VALUE
             'cessionariosAutorizados,array,10,01;Ý;,object,0;'.
             10 FILLER     PIC X(027)  VALUE
             'cpnjOuCpfCesAutd,string,14;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(045)  VALUE
             'renegociacoesDividas,array,10,01;Ý;,object,0;'.
             10 FILLER     PIC X(030)  VALUE
             'identdOpOrRenegcDiv,string,19;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
      *
           05  BVVEWMAP-E15-BODY.
             10 FILLER     PIC X(013)  VALUE
             'op,string,12;'.
             10 FILLER     PIC X(027)  VALUE
             'identdNegcRecbvl,string,40;'.
             10 FILLER     PIC X(028)  VALUE
             'indrCancelVlrTotal,string,1;'.
             10 FILLER     PIC X(022)  VALUE
             'indrLiquidOp,string,1;'.
             10 FILLER     PIC X(032)  VALUE
             'indrCancelCessConstitr,string,1;'.
      *
           05  BVVEWMAP-E18-BODY.
             10 FILLER     PIC X(027)  VALUE
             'identdNegcRecbvl,string,19;'.
             10 FILLER     PIC X(036)  VALUE
             'cnpjOuCnpjBaseOuCpfTitlar,string,14;'.
             10 FILLER     PIC X(036)  VALUE
             'identdOpDescstcNegcRecbvl,string,19;'.
             10 FILLER     PIC X(030)  VALUE
             'sitPedDescstcNegcRecbvl,int,2;'.
      *
           05  BVVEWMAP-R00-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
      *
           05  BVVEWMAP-R01-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(014)  VALUE
             'msg,string,50;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-R11-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(031)  VALUE
             'identdCtrlReqSolicte,string,20;'.
             10 FILLER     PIC X(026)  VALUE
             'identdCtrlOptIn,string,19;'.
             10 FILLER     PIC X(017)  VALUE
             'cnpjER,string,14;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-R13-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(027)  VALUE
             'identdCtrlOptOut,string,19;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-R27-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(026)  VALUE
             'identdOpContstc,string,19;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-R19-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(032)  VALUE
             'identdConjUniddRecbvl,string,19;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-X10-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(032)  VALUE
             'posicional,string,15000;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-R21-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(017)  VALUE
             'cnpjER,string,14;'.
             10 FILLER     PIC X(027)  VALUE
             'identdNegcRecbvl,string,40;'.
             10 FILLER     PIC X(019)  VALUE
             'identdOp,string,19;'.
             10 FILLER     PIC X(020)  VALUE
             'indrTpNegc,string,2;'.
             10 FILLER     PIC X(019)  VALUE
             'dtVencOp,string,10;'.
             10 FILLER     PIC X(031)  VALUE
             'vlrTotLimOuSldDevdr,num-comp,4;'.
             10 FILLER     PIC X(018)  VALUE
             'vlrGar,num-comp,4;'.
             10 FILLER     PIC X(020)  VALUE
             'indrGestER,string,1;'.
             10 FILLER     PIC X(022)  VALUE
             'indrRegrDivs,string,1;'.
             10 FILLER     PIC X(036)  VALUE
             'indrAlcancContrtoCreddrSub,string,1;'.
             10 FILLER     PIC X(027)  VALUE
             'indrActeIncondlOp,string,1;'.
             10 FILLER     PIC X(033)  VALUE
             'identdCIPOpOrRenegcDiv,string,19;'.
             10 FILLER     PIC X(036)  VALUE
             'indrActeUniddRecbvlReserv,string,01;'.
             10 FILLER     PIC X(019)  VALUE
             'dtHrIncl,string,24;'.
             10 FILLER     PIC X(020)  VALUE
             'indrSitOp,string,01;'.
             10 FILLER     PIC X(023)  VALUE
             'indrAutcCess,string,01;'.
             10 FILLER     PIC X(048)  VALUE
             'cessionariosAutorizados,array,10,01;Ý;,object,0;'.
             10 FILLER     PIC X(027)  VALUE
             'cnpjOuCpfCesAutd,string,14;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(048)  VALUE
             'renegociacoesDividas,array,10,01;Ý;,object,0;'.
             10 FILLER     PIC X(030)  VALUE
             'identdOpOrRenegcDiv,string,19;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(028)  VALUE
             'unidadesRecebiveis,object,1;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(016)  VALUE
             'href,string,256;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(039)  VALUE
             'unidadesRecebiveisAConstituir,object,1;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(016)  VALUE
             'href,string,256;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
0121       05  BVVEWMAP-O21-JSON.
              10 BVVEWMAP-O21-HEADER.
                15 FILLER  PIC X(018)  VALUE
                'controle,object,0;'.
                15 FILLER  PIC X(002)  VALUE
                '{;'.
                15 FILLER  PIC X(018)  VALUE
                'leiaute,string,08;'.
                15 FILLER  PIC X(013)  VALUE
                'id,string,20;'.
                15 FILLER  PIC X(014)  VALUE
                'codhttp,int,3;'.
                15 FILLER  PIC X(013)  VALUE
                'numpag,int,5;'.
                15 FILLER  PIC X(013)  VALUE
                'totpag,int,5;'.
                15 FILLER  PIC X(034)  VALUE
                'mensagens,array,05,04;Ý;,object,1;'.
                15 FILLER  PIC X(017)  VALUE
                'codigo,string,10;'.
                15 FILLER  PIC X(020)  VALUE
                'conteudo,string,100;'.
                15 FILLER  PIC X(016)  VALUE
                'campo,string,50;'.
                15 FILLER  PIC X(020)  VALUE
                'mensagem,string,200;'.
                15 FILLER  PIC X(002)  VALUE
                '¨;'.
                15 FILLER  PIC X(002)  VALUE
                '};'.
      *
              10 BVVEWMAP-O21-BODY.
                15 FILLER  PIC X(030)  VALUE
                'dados,array,50,12;Ý;,object,0;'.
                15 FILLER  PIC X(036)  VALUE
                'cnpjOuCnpjBaseOuCpfTitlar,string,14;'.
                15 FILLER  PIC X(028)  VALUE
                'cnpjOuCpfTitlarCt,string,14;'.
                15 FILLER  PIC X(024)  VALUE
                'ispbBcoRecbdr,string,08;'.
                15 FILLER  PIC X(014)  VALUE
                'tpCt,string,2;'.
                15 FILLER  PIC X(009)  VALUE
                'ag,int,4;'.
                15 FILLER  PIC X(010)  VALUE
                'ct,int,13;'.
                15 FILLER  PIC X(024)  VALUE
                'priorddNegcRecbvl,int,9;'.
                15 FILLER  PIC X(024)  VALUE
                'cnpjCreddrSub,string,14;'.
                15 FILLER  PIC X(034)  VALUE
                'cnpjOuCpfUsuFinalRecbdr,string,14;'.
                15 FILLER  PIC X(030)  VALUE
                'codInstitdrArrajPgto,string,3;'.
                15 FILLER  PIC X(024)  VALUE
                'dtPrevtLiquid,string,10;'.
                15 FILLER  PIC X(020)  VALUE
                'vlrNegcd,num-comp,4;'.
                15 FILLER  PIC X(002)  VALUE
                '¨;'.
      *
           05  BVVEWMAP-A21-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(030)  VALUE
             'dados,array,20,13;Ý;,object,0;'.
             10 FILLER     PIC X(036)  VALUE
             'cnpjOuCnpjBaseOuCpfTitlar,string,14;'.
             10 FILLER     PIC X(028)  VALUE
             'cnpjOuCpfTitlarCt,string,14;'.
             10 FILLER     PIC X(024)  VALUE
             'ispbBcoRecbdr,string,08;'.
             10 FILLER     PIC X(014)  VALUE
             'tpCt,string,2;'.
             10 FILLER     PIC X(009)  VALUE
             'ag,int,4;'.
             10 FILLER     PIC X(010)  VALUE
             'ct,int,13;'.
             10 FILLER     PIC X(014)  VALUE
             'ctPgto,int,18;'.
             10 FILLER     PIC X(024)  VALUE
             'priorddNegcRecbvl,int,9;'.
             10 FILLER     PIC X(024)  VALUE
             'cnpjCreddrSub,string,14;'.
             10 FILLER     PIC X(052)  VALUE
             'cnpjOuCnpjBaseOuCpfUsuFinalRecbdr,string,14;'.
             10 FILLER     PIC X(030)  VALUE
             'codInstitdrArrajPgto,string,3;'.
             10 FILLER     PIC X(024)  VALUE
             'dtPrevtLiquid,string,10;'.
             10 FILLER     PIC X(032)  VALUE
             'vlrPercNegcdConstitr,num-comp,4;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
      *
           05  BVVEWMAP-R09-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(019)  VALUE
             'identdOp,string,19;'.
             10 FILLER     PIC X(018)  VALUE
             'operacao,object,1;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(016)  VALUE
             'href,string,256;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-R15-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(020)  VALUE
             'dtHrAbert,string,24;'.
             10 FILLER     PIC X(017)  VALUE
             'dtHrER,string,24;'.
             10 FILLER     PIC X(016)  VALUE
             'dtRef,string,10;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-R16-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(019)  VALUE
             'dtHrFcht,string,24;'.
             10 FILLER     PIC X(017)  VALUE
             'dtHrER,string,24;'.
             10 FILLER     PIC X(016)  VALUE
             'dtRef,string,10;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-R28-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(026)  VALUE
             'identdCtrlOptIn,string,19;'.
             10 FILLER     PIC X(052)  VALUE
             'cnpjOuCnpjBaseOuCpfUsuFinalRecbdrOuTitlar,string,14;'.
             10 FILLER     PIC X(024)  VALUE
             'cnpjCreddrSub,string,14;'.
             10 FILLER     PIC X(021)  VALUE
             'cnpjFincdr,string,14;'.
             10 FILLER     PIC X(030)  VALUE
             'codInstitdrArrajPgto,string,3;'.
             10 FILLER     PIC X(018)  VALUE
             'dtOptIn,string,10;'.
             10 FILLER     PIC X(021)  VALUE
             'dtIniOptIn,string,10;'.
             10 FILLER     PIC X(021)  VALUE
             'dtFimOptIn,string,10;'.
             10 FILLER     PIC X(019)  VALUE
             'indrDomcl,string,1;'.
             10 FILLER     PIC X(017)  VALUE
             'cnpjER,string,14;'.
             10 FILLER     PIC X(031)  VALUE
             'cpfOuCnpjRecbdrOptIn,string,14;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-R29-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(027)  VALUE
             'identdCtrlOptOut,string,19;'.
             10 FILLER     PIC X(026)  VALUE
             'identdCtrlOptIn,string,19;'.
             10 FILLER     PIC X(021)  VALUE
             'dtFimOptIn,string,10;'.
             10 FILLER     PIC X(032)  VALUE
             'cpfOuCnpjRecbdrOptOut,string,14;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-I19-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(027)  VALUE
             'identdNegcRecbvl,string,40;'.
             10 FILLER     PIC X(019)  VALUE
             'identdOp,string,19;'.
             10 FILLER     PIC X(016)  VALUE
             'sitRet,string,6;'.
1801  *      10 FILLER     PIC X(039)  VALUE
.     *      'unidadesRecebiveisDisponiveis,object,1;'.
.     *      10 FILLER     PIC X(002)  VALUE
.     *      '{;'.
.     *      10 FILLER     PIC X(016)  VALUE
.     *      'href,string,256;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-R20-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(027)  VALUE
             'identdNegcRecbvl,string,40;'.
             10 FILLER     PIC X(019)  VALUE
             'identdOp,string,19;'.
             10 FILLER     PIC X(016)  VALUE
             'sitRet,string,6;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-R14-BODY.
             10 FILLER     PIC X(018)  VALUE
             'controle,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(018)  VALUE
             'leiaute,string,08;'.
             10 FILLER     PIC X(013)  VALUE
             'id,string,20;'.
             10 FILLER     PIC X(014)  VALUE
             'codhttp,int,3;'.
             10 FILLER     PIC X(013)  VALUE
             'numpag,int,5;'.
             10 FILLER     PIC X(013)  VALUE
             'totpag,int,5;'.
             10 FILLER     PIC X(034)  VALUE
             'mensagens,array,05,04;Ý;,object,1;'.
             10 FILLER     PIC X(017)  VALUE
             'codigo,string,10;'.
             10 FILLER     PIC X(020)  VALUE
             'conteudo,string,100;'.
             10 FILLER     PIC X(016)  VALUE
             'campo,string,50;'.
             10 FILLER     PIC X(020)  VALUE
             'mensagem,string,200;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
             10 FILLER     PIC X(015)  VALUE
             'dados,object,0;'.
             10 FILLER     PIC X(002)  VALUE
             '{;'.
             10 FILLER     PIC X(017)  VALUE
             'cnpjER,string,14;'.
             10 FILLER     PIC X(027)  VALUE
             'identdNegcRecbvl,string,40;'.
             10 FILLER     PIC X(019)  VALUE
             'identdOp,string,23;'.
             10 FILLER     PIC X(036)  VALUE
             'cnpjOuCnpjBaseOuCpfTitlar,string,14;'.
             10 FILLER     PIC X(036)  VALUE
             'identdOpDescstcNegcRecbvl,string,19;'.
             10 FILLER     PIC X(044)  VALUE
             'cnpjCreddrSubPedDescstcNegcRecbvl,string,14;'.
             10 FILLER     PIC X(048)  VALUE
             'listaUnidadesRecebiveis,array,20,06;Ý;,object,0;'.
             10 FILLER     PIC X(024)  VALUE
             'cnpjCreddrSub,string,14;'.
             10 FILLER     PIC X(034)  VALUE
             'cnpjOuCpfUsuFinalRecbdr,string,14;'.
             10 FILLER     PIC X(026)  VALUE
             'cnpjOuCpfTitlar,string,14;'.
             10 FILLER     PIC X(030)  VALUE
             'codInstitdrArrajPgto,string,3;'.
             10 FILLER     PIC X(024)  VALUE
             'dtPrevtLiquid,string,10;'.
             10 FILLER     PIC X(021)  VALUE
             'vlrDescst,num-comp,4;'.
             10 FILLER     PIC X(002)  VALUE
             '¨;'.
             10 FILLER     PIC X(002)  VALUE
             '};'.
      *
           05  BVVEWMAP-PREFIX-URL.
             10 WRK-PREFIX-DEV     PIC X(037)  VALUE
             'https://apisandbox.cipr2c3.org.br/api'.
HOMOL        10 WRK-PREFIX-HOM     PIC X(034)  VALUE
PROD  *      10 WRK-PREFIX-PRD     PIC X(034)  VALUE
             'https://apihext.cipr2c3.org.br/api'.
HOMOL        10 WRK-PREFIX-PRD     PIC X(030)  VALUE
PROD  *      10 WRK-PREFIX-HOM     PIC X(030)  VALUE
             'https://api.cipr2c3.org.br/api'.
      *
           05  BVVEWMAP-PATH-URL.
             10 WRK-PATH-E01       PIC X(019)  VALUE
             '/v1/ferramentas/eco'.
             10 WRK-PATH-E02       PIC X(023)  VALUE
             '/v1/unidades-recebiveis'.
             10 WRK-PATH-E03       PIC X(013)  VALUE
             '/v1/anuencias'.
             10 WRK-PATH-E04.
               15 WRK-PATH-E04-PRE PIC X(014)  VALUE
               '/v1/anuencias/'.
               15 WRK-PATH-E04-ID  PIC X(019)  VALUE
               SPACES.
             10 WRK-PATH-E05       PIC X(016)  VALUE
             '/v1/contestacoes'.
             10 WRK-PATH-E06.
               15 WRK-PATH-E06-PRE PIC X(017)  VALUE
               '/v1/contestacoes/'.
               15 WRK-PATH-E06-ID  PIC X(019)  VALUE
               SPACES.
             10 WRK-PATH-E07       PIC X(046)  VALUE
             '/v1/financiadora/conjuntos-unidades-recebiveis'.
             10 WRK-PATH-E08.
               15 WRK-PATH-E08-PRE PIC X(047)  VALUE
               '/v1/financiadora/conjuntos-unidades-recebiveis/'.
               15 WRK-PATH-E08-ID  PIC X(019)  VALUE
               SPACES.
               15 WRK-PATH-E08-SUL PIC X(026)  VALUE
               '/lotes-unidades-recebiveis'.
             10 WRK-PATH-E09      PIC X(026)  VALUE
             '/v1/financiadora/operacoes'.
             10 WRK-PATH-E10      PIC X(026)  VALUE
             '/v1/financiadora/operacoes'.
             10 WRK-PATH-E11      PIC X(026)  VALUE
             '/v1/financiadora/operacoes'.
             10 WRK-PATH-E12.
               15 WRK-PATH-E12-PRE PIC X(027)  VALUE
               '/v1/financiadora/operacoes/'.
               15 WRK-PATH-E12-ID  PIC X(019)  VALUE
               SPACES.
             10 WRK-PATH-E13.
               15 WRK-PATH-E13-PRE PIC X(027)  VALUE
               '/v1/financiadora/operacoes/'.
               15 WRK-PATH-E13-ID  PIC X(019)  VALUE
               SPACES.
             10 WRK-PATH-E14.
               15 WRK-PATH-E14-PRE PIC X(027)  VALUE
               '/v1/financiadora/operacoes/'.
               15 WRK-PATH-E14-ID  PIC X(019)  VALUE
               SPACES.
             10 WRK-PATH-E15.
               15 WRK-PATH-E15-PRE PIC X(027)  VALUE
               '/v1/financiadora/operacoes/'.
               15 WRK-PATH-E15-ID  PIC X(019)  VALUE
               SPACES.
             10 WRK-PATH-E16.
               15 WRK-PATH-E16-PRE PIC X(027)  VALUE
               '/v1/financiadora/operacoes/'.
               15 WRK-PATH-E16-ID  PIC X(019)  VALUE
               SPACES.
               15 WRK-PATH-E16-SUL PIC X(020)  VALUE
               '/unidades-recebiveis'.
             10 WRK-PATH-E17.
               15 WRK-PATH-E17-PRE PIC X(027)  VALUE
               '/v1/financiadora/operacoes/'.
               15 WRK-PATH-E17-ID  PIC X(019)  VALUE
               SPACES.
               15 WRK-PATH-E17-SUL PIC X(033)  VALUE
               SPACES.
               15 WRK-PATH-E17-SUL PIC X(033)  VALUE
               '/unidades-recebiveis-a-constituir'.
             10 WRK-PATH-E18.
               15 WRK-PATH-E18-PRE PIC X(027)  VALUE
               '/v1/financiadora/operacoes/'.
               15 WRK-PATH-E18-ID  PIC X(019)  VALUE
               SPACES.
               15 WRK-PATH-E18-SUL PIC X(033)  VALUE
               '/recusas-desconstituicao-garantia'.
      *
